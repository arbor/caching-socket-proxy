module Main where

import App
import App.AWS.Env
import Arbor.Logger
import Conduit
import Control.Exception
import Control.Lens
import Control.Monad                        (void)
import Data.Conduit
import Data.Maybe                           (catMaybes)
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Conduit.Combinator
import Network.StatsD                       as S
import System.Environment

import qualified Control.Concurrent.STM      as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Data.ByteString             as BS
import qualified Data.Conduit                as C
import qualified Data.Conduit.List           as C
import qualified Data.Conduit.Network        as N
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Network.Socket              as N
import qualified Network.Socket.ByteString   as N
import qualified System.IO.Unsafe            as U

main :: IO ()
main = do
  opt <- parseOptions
  progName <- T.pack <$> getProgName
  let logLvk  = opt ^. optLogLevel
  let statsConf = opt ^. optStatsConfig

  withStdOutTimedFastLogger $ \lgr -> do
    withStatsClient progName statsConf $ \stats -> do
      envAws <- mkEnv (opt ^. optRegion) logLvk lgr
      let envApp = AppEnv opt envAws stats (Logger lgr logLvk)
      res <- runApplication opt envApp
      case res of
        Left err -> pushLogMessage lgr LevelError ("Exiting: " <> show err)
        Right _  -> pure ()
    pushLogMessage lgr LevelError ("Premature exit, must not happen." :: String)

gCache :: STM.TVar (M.Map BS.ByteString BS.ByteString)
gCache = U.unsafePerformIO $ STM.newTVarIO M.empty
{-# NOINLINE gCache #-}

runApplication :: Options -> AppEnv -> IO (Either AppError ())
runApplication opt envApp = do
  N.runTCPServer (N.serverSettings (opt ^. sourcePort) "") $ \appData -> do
    key <- runConduit (N.appSource appData .| foldC)

    mValue <- STM.atomically $ STM.readTVar gCache <&> M.lookup key

    case mValue of
      Just value -> runConduit (C.sourceList [value] .| N.appSink appData)
      Nothing    -> do
        addrInfo <- N.getAddrInfo Nothing (Just (opt ^. targetHost)) (Just $ show (opt ^. targetPort))
        let serverAddr = head addrInfo
        sock <- N.socket (N.addrFamily serverAddr) N.Stream N.defaultProtocol

        runConduit (C.sourceList [key] .| N.sinkSocket sock)
        value <- runConduit (N.sourceSocket sock .| foldC)

        STM.atomically $ STM.modifyTVar gCache (M.update (const (Just key)) value)

        runConduit (C.sourceList [value] .| N.appSink appData)

    return ()

  runApplicationM envApp $ do
    _ <- view appOptions
    return ()

---------------------- TO BE MOVED TO A LIBRARY -------------------------------
throwLeftC :: MonadAppError m => (e -> AppError) -> Conduit (Either e a) m (Either e a)
throwLeftC f = awaitForever $ \msg ->
  throwErrorAs f msg

throwLeftSatisfyC :: MonadAppError m => (e -> AppError) -> (e -> Bool) -> Conduit (Either e a) m (Either e a)
throwLeftSatisfyC f p = awaitForever $ \msg ->
  case msg of
    Right a -> yield (Right a)
    Left e  | p e -> throwErrorAs f (Left e)
    Left e  -> yield (Left e)
-------------------------------------------------------------------------------

withStatsClient :: AppName -> StatsConfig -> (StatsClient -> IO a) -> IO a
withStatsClient appName statsConf f = do
  globalTags <- mkStatsTags statsConf
  let statsOpts = DogStatsSettings (statsConf ^. statsHost) (statsConf ^. statsPort)
  bracket (createStatsClient statsOpts (MetricName appName) globalTags) closeStatsClient f

mkStatsTags :: StatsConfig -> IO [Tag]
mkStatsTags statsConf = do
  deplId <- envTag "TASK_DEPLOY_ID" "deploy_id"
  let envTags = catMaybes [deplId]
  return $ envTags <> (statsConf ^. statsTags <&> toTag)
  where
    toTag (StatsTag (k, v)) = S.tag k v

