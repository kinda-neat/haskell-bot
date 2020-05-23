{-# LANGUAGE OverloadedStrings #-}

module VKBot.Bot where

import qualified Bot
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (pack)
import Data.Either (either)
import Data.List.Split (splitOn)
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Network.HTTP.Req
import ReadConfig (VKConfig(..))
import VKBot.Types

getVKLongPollAccessInfo :: VKConfig -> IO Bot.BotPayload
getVKLongPollAccessInfo config = do
  res <- runReq defaultHttpConfig $ getVKLongPollAccessInfoReq config
  return $ parseLongPollAccessInfo res

getVKLongPollAccessInfoReq :: VKConfig -> Req (JsonResponse Value)
getVKLongPollAccessInfoReq config =
  req
    GET
    (https "api.vk.com" /: "method" /: "groups.getLongPollServer")
    NoReqBody
    jsonResponse
    ("group_id" =: vkGroupId config <> "access_token" =: vkToken config <> "v" =:
     T.pack "5.50")

parseLongPollAccessInfo :: JsonResponse Value -> Bot.BotPayload
parseLongPollAccessInfo val =
  case parseEither parseJSON (responseBody val) of
    Left e -> error e
    Right res -> (vkResponse res)

sendMessageVK :: VKConfig -> Bot.UserId -> Bot.MessageText -> IO ()
sendMessageVK config userId messageText = do
  _ <- runReq defaultHttpConfig $ sendMessageVKReq config userId messageText
  return ()

sendMessageVKReq ::
     VKConfig -> Bot.UserId -> Bot.MessageText -> Req IgnoreResponse
sendMessageVKReq config (Bot.UserId userId) messageText =
  req
    GET
    (https "api.vk.com" /: "method" /: "messages.send")
    NoReqBody
    ignoreResponse
    ("message" =: messageText <> "peer_id" =: userId <> "access_token" =:
     vkToken config <>
     "v" =:
     T.pack "5.50")

pollVK :: (VKConfig, Bot.BotPayload) -> IO ()
pollVK (config, payload) = do
  res <- getUpdates payload
  let updatesInfo = parsePollingRes $ res
      updates = vkPollUpdates updatesInfo
  if length updates > 0
    then do
      let lastUpdate = last updates
          messageText = vkUpdBody . vkUpdObject $ lastUpdate
          userId = Bot.UserId . vkUpdUserId . vkUpdObject $ lastUpdate
      putStrLn $ show lastUpdate
      echoMessageVK config 5 userId messageText
    else putStrLn "No updates"
  pollVK
    (config, payload {Bot.vkTs = (read $ vkPollTs updatesInfo :: Bot.Option)})

getUpdates :: Bot.BotPayload -> IO (JsonResponse Value)
getUpdates payload =
  runReq defaultHttpConfig $ req GET reqUrl NoReqBody jsonResponse reqOptions
  where
    splitUrl = splitOn "/" $ Bot.vkServer payload
    host = splitUrl !! 2
    resource = splitUrl !! 3
    reqUrl = https (T.pack host) /: (T.pack resource)
    reqOptions =
      ("act" =: T.pack "a_check" <> "key" =: Bot.vkKey payload <> "ts" =:
       Bot.vkTs payload <>
       "wait" =:
       T.pack "25")

echoMessageVK ::
     VKConfig -> Bot.Option -> Bot.UserId -> Bot.MessageText -> IO ()
echoMessageVK config times userId messageText =
  sequence_ $ replicate times $ echoMessage config userId messageText

echoMessage :: VKConfig -> Bot.UserId -> Bot.MessageText -> IO ()
echoMessage config userId messageText = do
  _ <- runReq defaultHttpConfig $ sendMessageVKReq config userId messageText
  return ()

parsePollingRes :: JsonResponse Value -> VKPollResp
parsePollingRes val = either error id $ parseEither parseJSON $ responseBody val
