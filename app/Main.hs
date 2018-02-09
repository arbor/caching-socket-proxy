{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import App
import App.AWS.Env
import Arbor.Logger
import Conduit
import Control.Concurrent                   hiding (yield)
import Control.Concurrent.MVar
import Control.Exception
import Control.Lens
import Control.Monad                        (void)
import Data.Conduit
import Data.Int
import Data.Maybe                           (catMaybes)
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Conduit.Combinator
import Network.StatsD                       as S
import System.Environment
import System.Timeout

import qualified Control.Concurrent.STM      as STM
import qualified Control.Concurrent.STM.TVar as STM
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.Conduit                as C
import qualified Data.Conduit.Binary         as CB
import qualified Data.Conduit.List           as C
import qualified Data.Conduit.Network        as N
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Network.Socket              as N
import qualified Network.Socket.ByteString   as N
import qualified System.IO                   as SIO
import qualified System.IO.Unsafe            as U

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

main :: IO ()
main = do
  opt <- parseOptions
  progName <- T.pack <$> getProgName
  let logLvk  = opt ^. optLogLevel
  let statsConf = opt ^. optStatsConfig

  withStdOutTimedFastLogger $ \lgr -> do
    pushLogMessage lgr LevelInfo $ "Starting with options: " <> show opt

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

collectCache :: TimedFastLogger -> Int -> IO ()
collectCache lgr interval = do
  threadDelay interval
  pushLogMessage lgr LevelInfo ("Clear cache" :: String)
  STM.atomically $ STM.writeTVar gCache M.empty

runQuery :: TimedFastLogger -> String -> Int -> BS.ByteString -> Int -> IO (Either String BS.ByteString)
runQuery lgr host port key maxDelay = do
  mEntry <- STM.atomically $ STM.readTVar gCache <&> M.lookup key

  case mEntry of
    Just value -> do
      pushLogMessage lgr LevelInfo $ "Cached: " <> show key <> " -> " <> show value
      return (Right value)
    Nothing    -> do
      doneVar <- newEmptyMVar
      addrInfo <- N.getAddrInfo Nothing (Just host) (Just $ show port)
      let serverAddr = head addrInfo
      _ <- bracket (N.socket (N.addrFamily serverAddr) N.Stream N.defaultProtocol) N.close $ \sock -> do
        r <- timeout maxDelay $ do
          N.connect sock (N.addrAddress serverAddr)

          N.setSocketOption sock N.NoDelay 1
          hdl <- N.socketToHandle sock SIO.ReadWriteMode
          SIO.hSetBuffering hdl SIO.NoBuffering

          runConduit (C.sourceList [key, "\n"] .| N.sinkSocket sock)
          value <- runConduit (N.sourceSocket sock .| foldC)
          pushLogMessage lgr LevelInfo $ "Remote: " <> show key <> " -> " <> show value
          putMVar doneVar ()

          STM.atomically $ STM.modifyTVar gCache (M.insert key value)

          return value
        _ <- forkIO $ do
          threadDelay maxDelay
          putMVar doneVar ()
          return ()
        return r
      mEntry2 <- STM.atomically $ STM.readTVar gCache <&> M.lookup key
      case mEntry2 of
        Just value -> return (Right value)
        Nothing    -> return (Left "Timed out")

runApplication :: Options -> AppEnv -> IO (Either AppError ())
runApplication opt envApp = do
  let lgr = envApp ^. lgLogger
  forkIO (collectCache lgr (opt ^. optCacheTtlSeconds * 1000 * 1000))
  N.runTCPServer (N.serverSettings (opt ^. optSourcePort) "*") $ \appData ->
    N.withSocketsDo $ do
      mKey     <- runConduit (N.appSource appData .| CB.lines .| C.head)
      case mKey of
        Just key -> do
          mValue  <- runQuery lgr (opt ^. optTargetHost) (opt ^. optTargetPort) key 1000000

          case mValue of
            Right value -> runConduit (C.sourceList [value] .| N.appSink appData)
            Left error -> do
              pushLogMessage lgr LevelInfo $ "Error: " <> show error
              runConduit (C.sourceList [] .| N.appSink appData)
        Nothing -> do
          putStrLn "Empty request"
          runConduit (C.sourceList [] .| N.appSink appData)

  runApplicationM envApp $ do
    _ <- view appOptions
    return ()

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
