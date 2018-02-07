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
import qualified Data.Conduit                as C
import qualified Data.Conduit.List           as C
import qualified Data.Conduit.Network        as N
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import qualified Network.Socket              as N
import qualified Network.Socket.ByteString   as N
import qualified System.IO.Unsafe            as U

{-# ANN module ("HLint: ignore Redundant do"  :: String) #-}

main :: IO ()
main = do
  opt <- parseOptions
  progName <- T.pack <$> getProgName
  let logLvk  = opt ^. optLogLevel
  let statsConf = opt ^. optStatsConfig

  putStrLn $ "Starting with options: " <> show opt

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

collectCache :: Int -> IO ()
collectCache interval = do
  threadDelay interval
  putStrLn "Clear cache"
  STM.atomically $ STM.writeTVar gCache M.empty

runQuery :: String -> Int -> BS.ByteString -> Int -> IO (Either String BS.ByteString)
runQuery host port key maxDelay = do
  mEntry <- STM.atomically $ STM.readTVar gCache <&> M.lookup key

  case mEntry of
    Just value -> do
      putStrLn $ "Cached: " <> show key <> " -> " <> show value
      return (Right value)
    Nothing    -> do
      doneVar <- newEmptyMVar
      addrInfo <- N.getAddrInfo Nothing (Just host) (Just $ show port)
      let serverAddr = head addrInfo
      result <- bracket (N.socket (N.addrFamily serverAddr) N.Stream N.defaultProtocol) N.close $ \sock -> do
        r <- timeout maxDelay $ do
          N.connect sock (N.addrAddress serverAddr)

          runConduit (C.sourceList [key] .| N.sinkSocket sock)
          value <- runConduit (N.sourceSocket sock .| foldC)
          putStrLn $ "Remote: " <> show key <> " -> " <> show value
          putMVar doneVar ()

          STM.atomically $ STM.modifyTVar gCache (M.insert key value)

          return value
        forkIO $ do
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
  forkIO (collectCache (opt ^. optCacheTtlSeconds * 1000 * 1000))
  N.runTCPServer (N.serverSettings (opt ^. optSourcePort) "*") $ \appData ->
    N.withSocketsDo $ do
      key     <- runConduit (N.appSource appData .| foldC)
      mValue  <- runQuery (opt ^. optTargetHost) (opt ^. optTargetPort) key 1000000

      case mValue of
        Right value -> runConduit (C.sourceList [value] .| N.appSink appData)
        Left error -> do
          putStrLn $ "Error: " <> show error
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
