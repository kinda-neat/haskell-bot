{-# LANGUAGE OverloadedStrings #-}

module Bot.Infrastructure.Telegram.Types where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T

data TResponse a = TResponse
  { ok :: Bool
  , result :: a
  } deriving (Show)

data TUpdate
  = TMessageUpdate { update_id :: Integer
                   , message :: TMessage }
  | TCallbackQueryUpdate { update_id :: Integer
                         , callbackQuery :: TCallbackQuery }
  deriving (Show)

data TMessage = TMessage
  { message_id :: Integer
  , text :: String
  , from :: TMessageFrom
  , mChat :: TChat
  } deriving (Show)

data TMessageFrom = TMessageFrom
  { messageFromId :: Integer
  , first_name :: String
  , username :: String
  } deriving (Show)

data TChat = TChat
  { chatId :: Integer
  } deriving (Show)

data TUser = TUser
  { uId :: Integer
  , uFirstName :: String
  } deriving (Show)

instance FromJSON TUser where
  parseJSON (Object o) = TUser <$> o .: "id" <*> o .: "first_name"

data TCallbackQuery = TCallbackQuery
  { cqId :: String
  , cqMessage :: TMessage
  , cqFromUser :: TUser
  , cqData :: String
  } deriving (Show)

data InlineKeyboardButton = InlineKeyboardButton
  { buttonText :: String
  , buttonValue :: Int
  }

instance (FromJSON a) => FromJSON (TResponse a) where
  parseJSON (Object o) = TResponse <$> o .: "ok" <*> o .: "result"

instance FromJSON TUpdate where
  parseJSON =
    withObject "TUpdate" $ \o -> do
      message <- (o .:? "message" :: Parser (Maybe TMessage))
      case message of
        Nothing ->
          TCallbackQueryUpdate <$> o .: "update_id" <*> o .: "callback_query"
        Just _ -> TMessageUpdate <$> o .: "update_id" <*> o .: "message"

instance FromJSON TMessage where
  parseJSON (Object o) =
    TMessage <$> o .: "message_id" <*> o .: "text" <*> o .: "from" <*>
    o .: "chat"

instance FromJSON TMessageFrom where
  parseJSON (Object o) =
    TMessageFrom <$> o .: "id" <*> o .: "first_name" <*> o .: "username"

instance FromJSON TChat where
  parseJSON (Object o) = TChat <$> o .: "id"

instance FromJSON TCallbackQuery where
  parseJSON (Object o) =
    TCallbackQuery <$> o .: "id" <*> o .: "message" <*> o .: "from" <*>
    o .: "data"

instance ToJSON InlineKeyboardButton where
  toJSON (InlineKeyboardButton {buttonText = text, buttonValue = callback_data}) =
    object ["text" .= text, "callback_data" .= callback_data]
