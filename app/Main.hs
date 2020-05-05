module Main where

import Bot
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromMaybe)
import ReadConfig
  ( Commands(..)
  , Config(..)
  , HelpCommand(..)
  , RepeatCommand(..)
  , readConfig
  )
import System.Environment
import System.IO
import TelegramBot.Bot
import UserPreferences
  ( createPrefsFileIfDontExist
  , getUserSpecifiedOption
  , saveSelectedOption
  )

data Env = Env
  { envConfig :: Config
  , envBotActions :: BotActions
  , envUserPrefsActions :: UserPrefsActions
  }

main :: IO ()
main = do
  (fileName:_) <- getArgs
  config <- readConfig fileName
  let telegramConfig = telegram config
      botActions =
        BotActions
          { runBot = runTelegramBot telegramConfig
          , showBotDescription = showTelegramBotDescription telegramConfig
          , askNumberToRepeatMessage =
              askNumberToRepeatMessageInTelegram telegramConfig
          , confirmSelectedOptionSaved =
              confirmSelectedOptionSavedInTelegram telegramConfig
          , replyToMessage = replyToTelegramMessage telegramConfig
          }
      userPrefsActions =
        UserPrefsActions
          { upSaveSelectedOption = saveSelectedOption
          , upGetUserSpecifiedOption = getUserSpecifiedOption
          }
      env =
        Env
          { envConfig = config
          , envBotActions = botActions
          , envUserPrefsActions = userPrefsActions
          }
  createPrefsFileIfDontExist
  runReaderT (runApp (TelegramRunBotPayload Nothing)) env

runApp :: BotPayload -> ReaderT Env IO ()
runApp botPayload = do
  config <- asks envConfig
  botActions <- asks envBotActions
  userPrefsActions <- asks envUserPrefsActions
  botRequest <- lift $ runBot botActions botPayload
  let botDescription = messageInReply . helpCommand . commands $ config
      question = questionText . repeatCommand . commands $ config
      replyTimes = selectedOption . repeatCommand . commands $ config
      options = possibleOptions . repeatCommand . commands $ config
      selectedOptByDefault = selectedOption . repeatCommand . commands $ config
  lift $
    case fst botRequest of
      (Help chatId) -> showBotDescription botActions chatId botDescription
      (Repeat chatId userId) -> do
        userOption <- getUserSpecifiedOption userId
        askNumberToRepeatMessage
          botActions
          chatId
          options
          question
          (fromMaybe selectedOptByDefault userOption)
      (SelectOption questionId userId replyTimes) -> do
        userOption <- getUserSpecifiedOption userId
        upSaveSelectedOption
          userPrefsActions
          userId
          (fromMaybe (fromMaybe selectedOptByDefault userOption) replyTimes)
        confirmSelectedOptionSaved
          botActions
          questionId
          "You're choice was saved!"
      (Message chatId userId msg) -> do
        userOption <- getUserSpecifiedOption userId
        replyToMessage
          botActions
          chatId
          (fromMaybe selectedOptByDefault userOption)
          msg
  runApp (snd botRequest)
