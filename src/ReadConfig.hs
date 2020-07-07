{-# LANGUAGE OverloadedStrings #-}

module ReadConfig
  ( readConfig
  , Config(..)
  , HelpCommand(..)
  , Commands(..)
  , RepeatCommand(..)
  , TelegramConfig(..)
  , VKConfig(..)
  ) where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V

data HelpCommand = HelpCommand
  { messageInReply :: String
  } deriving (Show)

data RepeatCommand = RepeatCommand
  { questionText :: String
  , possibleOptions :: [Int]
  , selectedOption :: Int
  } deriving (Show)

data Commands = Commands
  { helpCommand :: HelpCommand
  , repeatCommand :: RepeatCommand
  } deriving (Show)

data TelegramConfig = TelegramConfig
  { token :: String
  } deriving (Show)

data VKConfig = VKConfig
  { vkToken :: String
  , vkGroupId :: Integer
  } deriving (Show)

data Config = Config
  { commands :: Commands
  , telegram :: TelegramConfig
  , vk :: VKConfig
  } deriving (Show)

instance FromJSON TelegramConfig where
  parseJSON (Object o) = TelegramConfig <$> o .: "token"

instance FromJSON VKConfig where
  parseJSON (Object o) = VKConfig <$> o .: "token" <*> o .: "group_id"

instance FromJSON HelpCommand where
  parseJSON (Object o) = HelpCommand <$> o .: "messageInReply"

instance FromJSON RepeatCommand where
  parseJSON (Object o) =
    RepeatCommand <$> o .: "questionText" <*> o .: "possibleOptions" <*>
    o .: "selectedOption"

instance FromJSON Commands where
  parseJSON (Object o) = Commands <$> o .: "help" <*> o .: "repeat"

instance FromJSON Config where
  parseJSON (Object o) =
    Config <$> o .: "commands" <*> o .: "telegram" <*> o .: "vk"

readConfig :: String -> IO Config
readConfig fileName = do
  file <- B.readFile fileName
  return $ extractConfig file

extractConfig :: B.ByteString -> Config
extractConfig file =
  case decode file of
    Nothing -> error "could not decode a file"
    Just val ->
      case (parseMaybe parseJSON val) of
        Nothing -> error "could not extract a config values"
        Just config -> config
