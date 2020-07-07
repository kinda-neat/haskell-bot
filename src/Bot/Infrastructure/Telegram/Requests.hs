{-# LANGUAGE OverloadedStrings #-}

module Bot.Infrastructure.Telegram.Requests where

import Bot.Core.Telegram.Telegram
import Bot.Infrastructure.Telegram.Types
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.HTTP.Req

replyToTelegramMessage :: Token -> ChatId -> Int -> MessageText -> IO ()
replyToTelegramMessage config chatId times msg =
  runReq defaultHttpConfig $ echoMessageReq config chatId times msg

echoMessageReq :: Token -> ChatId -> Int -> MessageText -> Req ()
echoMessageReq config chatId times msg =
  mapM_
    (runReq defaultHttpConfig)
    (replicate times (sendMessageReq config chatId msg))

sendMessageReq :: Token -> ChatId -> MessageText -> Req (JsonResponse Value)
sendMessageReq token (ChatId chatId) text =
  req
    GET
    (https "api.telegram.org" /: (T.pack $ "bot" ++ token) /: "sendMessage")
    NoReqBody
    jsonResponse
    ("chat_id" =: chatId <> "text" =: text)

showTelegramBotDescription :: Token -> ChatId -> BotDescription -> IO ()
showTelegramBotDescription token chatId desc = do
  _ <- makeShowTelegramBotDescReq token chatId desc
  return ()

makeShowTelegramBotDescReq ::
     Token -> ChatId -> BotDescription -> IO (JsonResponse Value)
makeShowTelegramBotDescReq token chatId desc =
  runReq defaultHttpConfig $ sendMessageReq token chatId desc

makeButtons :: [Int] -> Value
makeButtons options =
  object
    [ "inline_keyboard" .=
      map (\option -> [InlineKeyboardButton (show option) option]) options
    ]

askNumberToRepeatMessageInTelegram ::
     Token -> [Int] -> QuestionText -> ChatId -> Int -> IO ()
askNumberToRepeatMessageInTelegram token options question (ChatId chatId) currentOption = do
  _ <-
    runReq defaultHttpConfig $
    req
      GET
      (https "api.telegram.org" /: (T.pack $ "bot" ++ token) /: "sendMessage")
      NoReqBody
      ignoreResponse
      ("chat_id" =: chatId <> "reply_markup" =:
       (T.decodeUtf8 $ encode (toJSON $ makeButtons options)) <>
       "text" =:
       question)
  return ()

confirmSelectedOptionSavedInTelegram ::
     Token -> QuestionId -> ConfirmText -> IO ()
confirmSelectedOptionSavedInTelegram token (QuestionId queryId) confirmText = do
  _ <-
    runReq defaultHttpConfig $
    req
      GET
      (https "api.telegram.org" /: (T.pack $ "bot" ++ token) /:
       "answerCallbackQuery")
      NoReqBody
      ignoreResponse
      ("callback_query_id" =: queryId <> "text" =: confirmText)
  return ()

makeGetUpdatesReq :: Token -> LastUpdateId -> IO (JsonResponse Value)
makeGetUpdatesReq token offset =
  runReq defaultHttpConfig $ getUpdatesReq token offset

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

parseGetUpdatesRes :: JsonResponse Value -> Either String (TResponse [TUpdate])
parseGetUpdatesRes updates = parseEither parseJSON (responseBody updates)

getNewLastUpdateId :: [TUpdate] -> Maybe Integer -> Maybe Integer
getNewLastUpdateId [] lastUpdateId = lastUpdateId
getNewLastUpdateId updates _ = Just ((+ 1) . update_id . last $ updates)
