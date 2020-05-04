module Main where

import Bot
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.Char (digitToInt, isDigit)
import ReadConfig
  ( Commands(..)
  , Config(..)
  , HelpCommand(..)
  , RepeatCommand(..)
  , readConfig
  )
import System.Environment
import TelegramBot.Bot

showBotDescriptionMock :: Integer -> String -> IO ()
showBotDescriptionMock _ desc = putStrLn desc

askNumberToRepeatMessageMock :: String -> Int -> IO Int
askNumberToRepeatMessageMock question repeatTimes = do
  putStrLn $ question ++ " Number of times: " ++ show repeatTimes
  rawRepeatTimes <- getChar
  let newRepeatTimes =
        if isDigit rawRepeatTimes
          then digitToInt rawRepeatTimes
          else repeatTimes
  return newRepeatTimes

replyToMessageMock :: Int -> String -> IO ()
replyToMessageMock times message = mapM_ putStrLn $ replicate times message

data Env = Env
  { envConfig :: Config
  , envBotActions :: BotActions
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
          , askNumberToRepeatMessage = askNumberToRepeatMessageMock
          , replyToMessage = replyToTelegramMessage telegramConfig
          }
      env = Env {envConfig = config, envBotActions = botActions}
  runReaderT (runApp (TelegramRunBotPayload Nothing)) env

runApp :: BotPayload -> ReaderT Env IO ()
runApp botPayload = do
  config <- asks envConfig
  botActions <- asks envBotActions
  botRequest <- lift $ runBot botActions botPayload
  let botDescription = messageInReply . helpCommand . commands $ config
      replyTimes = selectedOption . repeatCommand . commands $ config
  lift $
    case fst botRequest of
      (Help chatId) -> showBotDescription botActions chatId botDescription
      (Repeat chatId) -> do
        newRepeatTimes <- askNumberToRepeatMessage botActions "question" 5
        putStrLn $ "New repeat times " ++ show newRepeatTimes
        return ()
      (Message chatId msg) -> replyToMessage botActions chatId replyTimes msg
  runApp (snd botRequest)
