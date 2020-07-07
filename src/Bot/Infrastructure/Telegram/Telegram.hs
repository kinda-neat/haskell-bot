{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Bot.Infrastructure.Telegram.Telegram where

import Bot.Core
import Bot.Core.Telegram.Telegram
import Bot.Domain
import Bot.Infrastructure.Telegram.Requests
import Bot.Infrastructure.Telegram.Types
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import UserPreferences (getUserSpecifiedOption, saveSelectedOption)

newtype TelegramM a = TelegramM
  { unTelegramM :: ReaderT Env IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env)

-- rename currentOption -> optionSelectedByDefault (+ change in config)
instance EchoMessage TelegramM MessageTP where
  echoMessage (MessageTP chatId (UserId userId) messageText) = do
    token <- asks getToken
    userOption <- liftIO $ getUserSpecifiedOption $ show userId
    optionSelectedByDefault <- asks getCurrentOption
    liftIO $
      replyToTelegramMessage
        token
        chatId
        (fromMaybe optionSelectedByDefault userOption)
        messageText

instance ShowBotDescription TelegramM HelpTP where
  showBotDescription (HelpTP chatId) = do
    token <- asks getToken
    description <- asks getBotDescription
    liftIO $ showTelegramBotDescription token chatId description

instance AskTimesToEchoMessage TelegramM RepeatTP where
  askTimesToEchoMessage (RepeatTP chatId userId) = do
    token <- asks getToken
    options <- asks getOptions
    currentOption <- asks getCurrentOption
    question <- asks getQuestionText
    liftIO $
      askNumberToRepeatMessageInTelegram
        token
        options
        question
        chatId
        currentOption

instance SaveNewTimesToEcho TelegramM SelectOptionTP where
  saveNewTimesToEcho (SelectOptionTP questionId (UserId userId) option) = do
    optionSelectedByDefault <- asks getCurrentOption
    userOption <- liftIO $ getUserSpecifiedOption $ show userId
    liftIO $
      saveSelectedOption
        (show userId)
        (fromMaybe (fromMaybe optionSelectedByDefault userOption) option)

instance ConfirmTimesToEchoSaved TelegramM SelectOptionTP where
  confirmTimesToEchoSaved (SelectOptionTP questionId userId option) = do
    token <- asks getToken
    liftIO $
      confirmSelectedOptionSavedInTelegram
        token
        questionId
        "You're choice was saved!"

-- TP - Telegram Payload
instance AwaitCommand TelegramM RuntimeTP HelpTP RepeatTP MessageTP SelectOptionTP where
  awaitCommand runtimeInfo@(RuntimeTP lastUpdateId) = do
    token <- asks getToken
    lastUpdates <- liftIO $ makeGetUpdatesReq token lastUpdateId
    case parseGetUpdatesRes lastUpdates of
      Left e -> error "Could not parse updates from telegram."
      Right res -> do
        let updates = result res
            newLastUpdateId = getNewLastUpdateId updates lastUpdateId
        if newLastUpdateId == lastUpdateId
          then awaitCommand runtimeInfo
          else pure
                 ( runtimeInfo {tLastUpdateId = newLastUpdateId}
                 , identifyCommand $ last updates)
    where
      identifyCommand (TCallbackQueryUpdate {callbackQuery = TCallbackQuery { cqId = queryId
                                                                            , cqFromUser = cqFromUser
                                                                            , cqData = cqData
                                                                            }}) =
        SelectOption
          (SelectOptionTP
             (QuestionId queryId)
             userId
             (readMaybe cqData :: Maybe Int))
        where
          userId = UserId $ uId cqFromUser
      identifyCommand update
        | msg == "/help" = Help (HelpTP msgChatId)
        | msg == "/repeat" = Repeat (RepeatTP msgChatId userId)
        | otherwise = Message (MessageTP msgChatId userId msg)
        where
          msg = text . message $ update
          msgChatId = ChatId $ chatId . mChat . message $ update
          userId = UserId $ messageFromId . from . message $ update
