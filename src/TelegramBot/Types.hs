{-# LANGUAGE OverloadedStrings #-}

module TelegramBot.Types where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T

data TResponse a = TResponse
  { ok :: Bool
  , result :: a
  } deriving (Show)

data TUpdate = TUpdate
  { update_id :: Integer
  , message :: TMessage
  } deriving (Show)

data TMessage = TMessage
  { message_id :: Integer
  , text :: String
  , from :: TMessageFrom
  , mChat :: TChat
  } deriving (Show)

data TMessageFrom = TMessageFrom
  { id :: Integer
  , first_name :: String
  , last_name :: String
  , username :: String
  , language_code :: String
  } deriving (Show)

data TChat = TChat
  { chatId :: Integer
  } deriving (Show)

instance (FromJSON a) => FromJSON (TResponse a) where
  parseJSON (Object o) = TResponse <$> o .: "ok" <*> o .: "result"

instance FromJSON TUpdate where
  parseJSON (Object o) = TUpdate <$> o .: "update_id" <*> o .: "message"

instance FromJSON TMessage where
  parseJSON (Object o) =
    TMessage <$> o .: "message_id" <*> o .: "text" <*> o .: "from" <*>
    o .: "chat"

instance FromJSON TMessageFrom where
  parseJSON (Object o) =
    TMessageFrom <$> o .: "id" <*> o .: "first_name" <*> o .: "last_name" <*>
    o .: "username" <*>
    o .: "language_code"

instance FromJSON TChat where
  parseJSON (Object o) = TChat <$> o .: "id"
