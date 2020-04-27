{-# LANGUAGE OverloadedStrings #-}

module ReadingConfig where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B

data HelpCommand = HelpCommand
  { messageInReply :: String
  } deriving (Show)

data RepeatCommand = RepeatCommand
  { questionText :: String
  , possibleOptions :: [Int]
  , selectedOption :: Int
  } deriving (Show)

data Commands = Commands
  { help :: HelpCommand
  , repeat :: RepeatCommand
  } deriving (Show)

data Config = Config
  { commands :: Commands
  } deriving (Show)

instance FromJSON HelpCommand where
  parseJSON (Object o) =
    HelpCommand <$> o .: "messageInReply"

instance FromJSON RepeatCommand where
  parseJSON (Object o) =
    RepeatCommand <$> o .: "questionText"
                  <*> o .: "possibleOptions"
                  <*> o .: "selectedOption"

instance FromJSON Commands where
  parseJSON (Object o) =
    Commands <$> o .: "help"
             <*> o .: "repeat"

instance FromJSON Config where
  parseJSON (Object o) =
    Config <$> o .: "commands"

readConfig :: IO ()
readConfig = do
  file <- B.readFile "config.json" -- pass as cli arg
  let config = extractConfig file
  putStrLn $ show $ config
  return ()

extractConfig :: B.ByteString -> Config
extractConfig file = case decode file of
  Nothing -> error "cannot decode file"
  Just val -> case (parseMaybe parseJSON val) of
          Nothing -> error "could not extract config values"
          Just config -> config

