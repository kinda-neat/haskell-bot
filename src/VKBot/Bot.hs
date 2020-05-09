{-# LANGUAGE OverloadedStrings #-}

module VKBot.Bot where

import qualified Bot
import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Char8 (pack)
import Data.Maybe
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
