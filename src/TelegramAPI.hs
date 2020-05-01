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

pollServer :: TelegramConfig -> (Maybe Integer) -> IO ()
pollServer config offset =
  runReq (defaultHttpConfig {httpConfigProxy = makeProxyConfig config}) $ do
    s <-
      req
        GET
        (https "api.telegram.org" /: (T.pack $ "bot" ++ token config) /:
         "getUpdates")
        NoReqBody
        jsonResponse
        (if isJust offset
           then "offset" =: fromJust offset
           else mempty)
    let updates = parseEither parseGetUpdatesResponse (responseBody s)
    liftIO $
      case updates of
        Left e -> putStrLn $ show e
        Right _updates -> do
          let newLastUpdateId = getNewLastUpdateId (result _updates) offset
          putStrLn $ "Polling server..."
          if newLastUpdateId == offset
            then pollServer config offset
            else do
              let lastMessage = message . last $ result _updates
              let messageFrom = from lastMessage
              putStrLn $
                show $
                "New message: " ++
                text lastMessage ++
                " from username = " ++
                username messageFrom ++
                " and name = " ++
                first_name messageFrom ++ " " ++ last_name messageFrom
              putStrLn $ "Made a new request"
              pollServer config newLastUpdateId

parseGetUpdatesResponse :: Value -> Parser (TResponse [TUpdate])
parseGetUpdatesResponse = parseJSON

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
  } deriving (Show)

instance FromJSON TMessage where
  parseJSON (Object o) =
    TMessage <$> o .: "message_id" <*> o .: "text" <*> o .: "from"

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
