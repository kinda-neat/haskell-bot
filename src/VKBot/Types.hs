{-# LANGUAGE OverloadedStrings #-}

module VKBot.Types where

import qualified Bot

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T

data VKPollAccessInfo = VKPollAccessInfo
  { vkResponse :: Bot.BotPayload
  }

data VKPollResp = VKPollResp
  { vkPollTs :: String
  , vkPollUpdates :: [VKPollUpdate]
  } deriving (Show)

data VKPollUpdate = VKPollUpdate
  { vkUpdType :: String
  , vkUpdGroupId :: Integer
  , vkUpdObject :: VKUpdateObject
  } deriving (Show)

data VKUpdateObject = VKUpdateObject
  { vkUpdBody :: String
  , vkUpdUserId :: Integer
  } deriving (Show)

instance FromJSON VKPollAccessInfo where
  parseJSON (Object o) = VKPollAccessInfo <$> o .: "response"

instance FromJSON VKPollResp where
  parseJSON (Object o) = VKPollResp <$> o .: "ts" <*> o .: "updates"

instance FromJSON VKPollUpdate where
  parseJSON (Object o) =
    VKPollUpdate <$> o .: "type" <*> o .: "group_id" <*> o .: "object"

instance FromJSON VKUpdateObject where
  parseJSON (Object o) = VKUpdateObject <$> o .: "body" <*> o .: "user_id"

instance FromJSON Bot.BotPayload where
  parseJSON (Object o) =
    Bot.VKBotPayload <$> o .: "key" <*> o .: "server" <*> o .: "ts"
