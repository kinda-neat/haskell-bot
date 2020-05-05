{-# LANGUAGE OverloadedStrings #-}

module TelegramBot.Bot where

import qualified Bot
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Text
import Data.Aeson.Types
import Data.ByteString.Char8 (pack)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.HTTP.Client (Proxy(..))
import Network.HTTP.Req
import ReadConfig (RepeatCommand(..), TelegramConfig(..), TelegramProxy(..))
import TelegramBot.Types
import Text.Read

makeProxyConfig :: TelegramConfig -> Maybe Proxy
makeProxyConfig config =
  Just $ Proxy {proxyHost = proxyHost, proxyPort = proxyPort}
  where
    proxyHost = pack $ proxyHostForTelegram $ proxy config
    proxyPort = proxyPortForTelegram $ proxy config

makeHttpConfig :: TelegramConfig -> HttpConfig
makeHttpConfig config =
  defaultHttpConfig {httpConfigProxy = makeProxyConfig config}

runTelegramBot ::
     TelegramConfig -> Bot.BotPayload -> IO (Bot.BotCommand, Bot.BotPayload)
runTelegramBot config payload@(Bot.TelegramRunBotPayload lastUpdateId) = do
  putStrLn "Polling server..."
  lastUpdates <- makeGetUpdatesReq config lastUpdateId
  case parseGetUpdatesRes lastUpdates of
    Left e -> error "Could not parse updates from telegram."
    Right res -> do
      let updates = result res
          newLastUpdateId = getNewLastUpdateId updates lastUpdateId
      if newLastUpdateId == lastUpdateId
        then runTelegramBot config payload
        else return
               ( identifyCommand $ last updates
               , Bot.TelegramRunBotPayload newLastUpdateId)

identifyCommand :: TUpdate -> Bot.BotCommand
identifyCommand (TCallbackQueryUpdate {callbackQuery = TCallbackQuery { cqId = queryId
                                                                      , cqFromUser = cqFromUser
                                                                      , cqData = cqData
                                                                      }}) =
  Bot.SelectOption queryId userId (readMaybe cqData :: Maybe Int)
  where
    userId = uId cqFromUser
identifyCommand update
  | msg == "/help" = Bot.Help msgChatId
  | msg == "/repeat" = Bot.Repeat msgChatId userId
  | otherwise = Bot.Message msgChatId userId msg
  where
    msg = text . message $ update
    msgChatId = chatId . mChat . message $ update
    userId = messageFromId . from . message $ update

makeGetUpdatesReq :: TelegramConfig -> Maybe Integer -> IO (JsonResponse Value)
makeGetUpdatesReq config offset =
  runReq (makeHttpConfig config) $ getUpdatesReq (token config) offset

parseGetUpdatesRes :: JsonResponse Value -> Either String (TResponse [TUpdate])
parseGetUpdatesRes updates = parseEither parseJSON (responseBody updates)

showTelegramBotDescription :: TelegramConfig -> Integer -> String -> IO ()
showTelegramBotDescription config chatId desc = do
  _ <- makeShowTelegramBotDescReq config chatId desc
  return ()

makeShowTelegramBotDescReq ::
     TelegramConfig -> Integer -> String -> IO (JsonResponse Value)
makeShowTelegramBotDescReq config chatId desc =
  runReq (makeHttpConfig config) $ sendMessageReq config chatId desc

replyToTelegramMessage :: TelegramConfig -> Integer -> Int -> String -> IO ()
replyToTelegramMessage config chatId times msg =
  runReq (makeHttpConfig config) $ echoMessageReq config chatId times msg

logMe :: String -> IO ()
logMe = putStrLn

echoMessageReq :: TelegramConfig -> Integer -> Int -> String -> Req ()
echoMessageReq config chatId times msg =
  mapM_
    (runReq (makeHttpConfig config))
    (replicate times (sendMessageReq config chatId msg))

makeMessageFrom :: TMessage -> TMessageFrom -> String
makeMessageFrom msg msgFrom =
  "New message: " ++
  text msg ++
  " from username = " ++
  username msgFrom ++ " and name = " ++ first_name msgFrom

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

getNewLastUpdateId :: [TUpdate] -> Maybe Integer -> Maybe Integer
getNewLastUpdateId [] lastUpdateId = lastUpdateId
getNewLastUpdateId updates _ = Just ((+ 1) . update_id . last $ updates)

makeButtons :: [Bot.Option] -> Value
makeButtons options =
  object
    [ "inline_keyboard" .=
      map (\option -> [InlineKeyboardButton (show option) option]) options
    ]

confirmSelectedOptionSavedInTelegram ::
     TelegramConfig -> String -> String -> IO ()
confirmSelectedOptionSavedInTelegram config queryId confirmText = do
  _ <-
    runReq (makeHttpConfig config) $
    req
      GET
      (https "api.telegram.org" /: (T.pack $ "bot" ++ token config) /:
       "answerCallbackQuery")
      NoReqBody
      ignoreResponse
      ("callback_query_id" =: queryId <> "text" =: confirmText)
  return ()

askNumberToRepeatMessageInTelegram ::
     TelegramConfig -> Integer -> [Bot.Option] -> String -> Bot.Option -> IO ()
askNumberToRepeatMessageInTelegram config chatId options question currentOption = do
  _ <-
    runReq (makeHttpConfig config) $
    req
      GET
      (https "api.telegram.org" /: (T.pack $ "bot" ++ token config) /:
       "sendMessage")
      NoReqBody
      ignoreResponse
      ("chat_id" =: chatId <> "reply_markup" =:
       (T.decodeUtf8 $ encode (toJSON $ makeButtons options)) <>
       "text" =:
       question)
  return ()

parseSelectedOption :: JsonResponse Value -> Int -> Int
parseSelectedOption val currentOption =
  fromMaybe currentOption $ parseMaybe parseJSON (responseBody val)
