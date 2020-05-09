{-# LANGUAGE OverloadedStrings #-}

module VKBot.Types where

import qualified Bot

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T

data VKResponse = VKResponse
  { vkResponse :: Bot.BotPayload
  }

instance FromJSON VKResponse where
  parseJSON (Object o) = VKResponse <$> o .: "response"

instance FromJSON Bot.BotPayload where
  parseJSON (Object o) =
    Bot.VKBotPayload <$> o .: "key" <*> o .: "server" <*> o .: "ts"
