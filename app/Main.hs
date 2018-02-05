module Main where

import App
import App.AWS.Env
import Arbor.Logger
import Control.Exception
import Control.Lens
import Data.Maybe         (catMaybes)
import Data.Semigroup     ((<>))
import Network.StatsD     as S
import System.Environment

import qualified Data.Text as T

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
      runApplication envApp
    pushLogMessage lgr LevelError ("Premature exit, must not happen." :: String)

runApplication :: AppEnv -> IO ()
runApplication envApp =
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

