{-# LANGUAGE OverloadedStrings #-}

module TelegramAPI where

import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (pack)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.HTTP.Client (Proxy(..))
import Network.HTTP.Req
import ReadConfig (TelegramConfig(..), TelegramProxy(..))

makeProxyConfig :: TelegramConfig -> Maybe Proxy
makeProxyConfig config =
  Just $ Proxy {proxyHost = proxyHost, proxyPort = proxyPort}
  where
    proxyHost = pack $ proxyHostForTelegram $ proxy config
    proxyPort = proxyPortForTelegram $ proxy config

makeHttpConfig :: TelegramConfig -> HttpConfig
makeHttpConfig config =
  defaultHttpConfig {httpConfigProxy = makeProxyConfig config}

pollServer :: TelegramConfig -> (Maybe Integer) -> IO ()
pollServer config offset =
  runReq httpConfig $ do
    s <- getUpdatesReq (token config) offset
    case (parseEither parseGetUpdatesRes (responseBody s)) of
      Left e -> liftIO $ logMe e
      Right updates -> do
        let newLastUpdateId = getNewLastUpdateId (result updates) offset
        if newLastUpdateId == offset
          then do
            liftIO $ logMe "No updates. Polling server..."
            liftIO $ pollServer config offset
          else do
            let lastMessage = message . last $ result updates
                messageFrom = from lastMessage
                lastMessageChatId = chatId . mChat $ lastMessage
            echoMessageReq 5 config lastMessageChatId lastMessage
            liftIO $ logMe $ makeMessageFrom lastMessage messageFrom
            liftIO $ logMe "Got new updates. Polling server..."
            liftIO $ pollServer config newLastUpdateId
  where
    httpConfig = (makeHttpConfig config)

logMe :: String -> IO ()
logMe = putStrLn

echoMessageReq :: Int -> TelegramConfig -> Integer -> TMessage -> Req ()
echoMessageReq times config chatId message =
  mapM_
    (runReq (makeHttpConfig config))
    (replicate
       times
       (sendMessageReq config chatId ("Got message from you: " ++ text message)))

makeMessageFrom :: TMessage -> TMessageFrom -> String
makeMessageFrom msg msgFrom =
  "New message: " ++
  text msg ++
  " from username = " ++
  username msgFrom ++
  " and name = " ++ first_name msgFrom ++ " " ++ last_name msgFrom

getUpdatesReq :: String -> (Maybe Integer) -> Req (JsonResponse Value)
getUpdatesReq token offset =
  req
    GET
    (https "api.telegram.org" /: (T.pack $ "bot" ++ token) /: "getUpdates")
    NoReqBody
    jsonResponse
    (if isJust offset
       then "offset" =: fromJust offset
       else mempty)

sendMessageReq ::
     TelegramConfig -> Integer -> String -> Req (JsonResponse Value)
sendMessageReq config chatId text =
  req
    GET
    (https "api.telegram.org" /: (T.pack $ "bot" ++ token config) /:
     "sendMessage")
    NoReqBody
    jsonResponse
    ("chat_id" =: chatId <> "text" =: text)

parseGetUpdatesRes :: Value -> Parser (TResponse [TUpdate])
parseGetUpdatesRes = parseJSON

getNewLastUpdateId :: [TUpdate] -> Maybe Integer -> Maybe Integer
getNewLastUpdateId [] lastUpdateId = lastUpdateId
getNewLastUpdateId updates _ = Just ((+ 1) . update_id . last $ updates)

extractAllTexts :: [TUpdate] -> [String]
extractAllTexts = foldr (\x xs -> (text . message $ x) : xs) []

data TResponse a = TResponse
  { ok :: Bool
  , result :: a
  } deriving (Show)

instance (FromJSON a) => FromJSON (TResponse a) where
  parseJSON (Object o) = TResponse <$> o .: "ok" <*> o .: "result"

data TUpdate = TUpdate
  { update_id :: Integer
  , message :: TMessage
  } deriving (Show)

instance FromJSON TUpdate where
  parseJSON (Object o) = TUpdate <$> o .: "update_id" <*> o .: "message"

data TMessage = TMessage
  { message_id :: Integer
  , text :: String
  , from :: TMessageFrom
  , mChat :: TChat
  } deriving (Show)

instance FromJSON TMessage where
  parseJSON (Object o) =
    TMessage <$> o .: "message_id" <*> o .: "text" <*> o .: "from" <*>
    o .: "chat"

data TMessageFrom = TMessageFrom
  { id :: Integer
  , first_name :: String
  , last_name :: String
  , username :: String
  , language_code :: String
  } deriving (Show)

instance FromJSON TMessageFrom where
  parseJSON (Object o) =
    TMessageFrom <$> o .: "id" <*> o .: "first_name" <*> o .: "last_name" <*>
    o .: "username" <*>
    o .: "language_code"

data TChat = TChat
  { chatId :: Integer
  } deriving (Show)

instance FromJSON TChat where
  parseJSON (Object o) = TChat <$> o .: "id"
