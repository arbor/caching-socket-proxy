{-# LANGUAGE TemplateHaskell #-}
module App.Options where

import Control.Lens
import Control.Monad.Logger  (LogLevel (..))
import Data.Semigroup        ((<>))
import Data.Text             (Text)
import Network.AWS.Data.Text (FromText (..), fromText)
import Network.AWS.S3.Types  (Region (..))
import Network.Socket        (HostName)
import Network.StatsD        (SampleRate (..))
import Options.Applicative
import Text.Read             (readEither)

import qualified Data.Text   as T
import qualified Network.AWS as AWS

newtype StatsTag = StatsTag (Text, Text) deriving (Show, Eq)

data StatsConfig = StatsConfig
  { _statsHost       :: HostName
  , _statsPort       :: Int
  , _statsTags       :: [StatsTag]
  , _statsSampleRate :: SampleRate
  } deriving (Show)

data Options = Options
  { _optLogLevel    :: LogLevel
  , _optRegion      :: Region
  , _sourcePort     :: Int
  , _targetHost     :: HostName
  , _targetPort     :: Int
  , _optStatsConfig :: StatsConfig
  } deriving (Show)

makeClassy ''StatsConfig
makeClassy ''Options

instance HasStatsConfig Options where
  statsConfig = optStatsConfig

statsConfigParser :: Parser StatsConfig
statsConfigParser = StatsConfig
  <$> strOption
      (  long "statsd-host"
      <> metavar "HOST_NAME"
      <> showDefault <> value "127.0.0.1"
      <> help "StatsD host name or IP address")
  <*> readOption
      (  long "statsd-port"
      <> metavar "PORT"
      <> showDefault <> value 8125
      <> help "StatsD port"
      <> hidden)
  <*> ( string2Tags <$> strOption
        (  long "statsd-tags"
        <> metavar "TAGS"
        <> showDefault <> value []
        <> help "StatsD tags"))
  <*> ( SampleRate <$> readOption
        (  long "statsd-sample-rate"
        <> metavar "SAMPLE_RATE"
        <> showDefault <> value 0.01
        <> help "StatsD sample rate"))

optParser :: Parser Options
optParser = Options
  <$> readOptionMsg "Valid values are LevelDebug, LevelInfo, LevelWarn, LevelError"
      (  long "log-level"
      <> metavar "LOG_LEVEL"
      <> showDefault <> value LevelInfo
      <> help "Log level")
  <*> readOrFromTextOption
      (  long "region"
      <> metavar "AWS_REGION"
      <> showDefault <> value Oregon
      <> help "The AWS region in which to operate"
      )
  <*> readOption
      (  long "listening-port"
      <> metavar "PORT"
      <> help "Listening port"
      <> hidden)
  <*> strOption
      (  long "remote-host"
      <> metavar "HOST_NAME"
      <> showDefault <> value "127.0.0.1"
      <> help "Remote hostname")
  <*> readOption
      (  long "remote-port"
      <> metavar "PORT"
      <> help "Remote port"
      <> hidden)
  <*> statsConfigParser

awsLogLevel :: Options -> AWS.LogLevel
awsLogLevel o = case o ^. optLogLevel of
  LevelError -> AWS.Error
  LevelWarn  -> AWS.Error
  LevelInfo  -> AWS.Error
  LevelDebug -> AWS.Info
  _          -> AWS.Trace

readOption :: Read a => Mod OptionFields a -> Parser a
readOption = option $ eitherReader readEither

readOptionMsg :: Read a => String -> Mod OptionFields a -> Parser a
readOptionMsg msg = option $ eitherReader (either (Left . const msg) Right . readEither)

readOrFromTextOption :: (Read a, FromText a) => Mod OptionFields a -> Parser a
readOrFromTextOption =
  let fromStr s = readEither s <|> fromText (T.pack s)
  in option $ eitherReader fromStr

string2Tags :: String -> [StatsTag]
string2Tags s = StatsTag . splitTag <$> splitTags
  where
    splitTags = T.split (==',') (T.pack s)
    splitTag t = T.drop 1 <$> T.break (==':') t

optParserInfo :: ParserInfo Options
optParserInfo = info (helper <*> optParser)
  (  fullDesc
  <> progDesc "For each attack caclulates its spuriousity index [0..1]"
  <> header "Spurious Attacks Detector"
  )

parseOptions :: IO Options
parseOptions = execParser optParserInfo
