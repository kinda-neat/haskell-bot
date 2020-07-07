{-# LANGUAGE OverloadedStrings #-}

module TelegramBot.Bot where

import qualified Bot
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (pack)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.HTTP.Req
import ReadConfig (RepeatCommand(..), TelegramConfig(..))
import TelegramBot.Types
import Text.Read

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
  Bot.SelectOption
    (Bot.QuestionId queryId)
    userId
    (readMaybe cqData :: Maybe Int)
  where
    userId = (Bot.UserId $ uId cqFromUser)
identifyCommand update
  | msg == "/help" = Bot.Help msgChatId
  | msg == "/repeat" = Bot.Repeat msgChatId userId
  | otherwise = Bot.Message msgChatId userId msg
  where
    msg = text . message $ update
    msgChatId = Bot.ChatId $ chatId . mChat . message $ update
    userId = Bot.UserId $ messageFromId . from . message $ update

makeGetUpdatesReq :: TelegramConfig -> Maybe Integer -> IO (JsonResponse Value)
makeGetUpdatesReq config offset =
  runReq defaultHttpConfig $ getUpdatesReq (token config) offset

parseGetUpdatesRes :: JsonResponse Value -> Either String (TResponse [TUpdate])
parseGetUpdatesRes updates = parseEither parseJSON (responseBody updates)

showTelegramBotDescription ::
     TelegramConfig -> Bot.ChatId -> Bot.BotDescription -> IO ()
showTelegramBotDescription config chatId desc = do
  _ <- makeShowTelegramBotDescReq config chatId desc
  return ()

makeShowTelegramBotDescReq ::
     TelegramConfig
  -> Bot.ChatId
  -> Bot.BotDescription
  -> IO (JsonResponse Value)
makeShowTelegramBotDescReq config chatId desc =
  runReq defaultHttpConfig $ sendMessageReq config chatId desc

replyToTelegramMessage ::
     TelegramConfig -> Bot.ChatId -> Bot.Option -> Bot.MessageText -> IO ()
replyToTelegramMessage config chatId times msg =
  runReq defaultHttpConfig $ echoMessageReq config chatId times msg

echoMessageReq ::
     TelegramConfig -> Bot.ChatId -> Bot.Option -> Bot.MessageText -> Req ()
echoMessageReq config chatId times msg =
  mapM_
    (runReq defaultHttpConfig)
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
     TelegramConfig -> Bot.ChatId -> Bot.MessageText -> Req (JsonResponse Value)
sendMessageReq config (Bot.ChatId chatId) text =
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
     TelegramConfig -> Bot.QuestionId -> Bot.ConfirmText -> IO ()
confirmSelectedOptionSavedInTelegram config (Bot.QuestionId queryId) confirmText = do
  _ <-
    runReq defaultHttpConfig $
    req
      GET
      (https "api.telegram.org" /: (T.pack $ "bot" ++ token config) /:
       "answerCallbackQuery")
      NoReqBody
      ignoreResponse
      ("callback_query_id" =: queryId <> "text" =: confirmText)
  return ()

askNumberToRepeatMessageInTelegram ::
     TelegramConfig
  -> [Bot.Option]
  -> Bot.QuestionText
  -> Bot.ChatId
  -> Bot.Option
  -> IO ()
askNumberToRepeatMessageInTelegram config options question (Bot.ChatId chatId) currentOption = do
  _ <-
    runReq defaultHttpConfig $
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
