module Bot.Core.Telegram.Telegram where

import Bot.Core
import ReadConfig
  ( Commands(..)
  , Config(..)
  , HelpCommand(..)
  , RepeatCommand(..)
  , TelegramConfig(..)
  )

getTimesToEcho :: Env -> Int
getTimesToEcho = selectedOption . repeatCommand . commands . envConfig

getOptions :: Env -> [Int]
getOptions = possibleOptions . repeatCommand . commands . envConfig

getCurrentOption :: Env -> Int
getCurrentOption = selectedOption . repeatCommand . commands . envConfig

getQuestionText :: Env -> String
getQuestionText = questionText . repeatCommand . commands . envConfig

getBotDescription :: Env -> String
getBotDescription = messageInReply . helpCommand . commands . envConfig

getToken :: Env -> Token
getToken = token . telegram . envConfig

type LastUpdateId = Maybe Integer

type Token = String

newtype ChatId =
  ChatId Integer

newtype UserId =
  UserId Integer

newtype QuestionId =
  QuestionId String

type Option = Int

type BotDescription = String

type QuestionText = String

type ConfirmText = String

type MessageText = String

data HelpTP =
  HelpTP ChatId

data RepeatTP =
  RepeatTP ChatId
           UserId

data MessageTP =
  MessageTP ChatId
            UserId
            MessageText

data SelectOptionTP =
  SelectOptionTP QuestionId
                 UserId
                 (Maybe Int)

data RuntimeTP = RuntimeTP
  { tLastUpdateId :: LastUpdateId
  } deriving (Show)
