module Bot where

data BotCommand
  = Help ChatId
  | Repeat ChatId
  | Message ChatId
            String
  deriving (Show)

data BotActions = BotActions
  { runBot :: BotPayload -> IO (BotCommand, BotPayload)
  , showBotDescription :: ChatId -> String -> IO ()
  , askNumberToRepeatMessage :: String -> Int -> IO Int
  , replyToMessage :: ChatId -> ReplyTimes -> String -> IO ()
  }

type LastUpdateId = Maybe Integer

type ChatId = Integer

type ReplyTimes = Int

data BotPayload
  = TelegramRunBotPayload LastUpdateId
  | TelegramShowDescPayload Integer
  | VKBotPayload
  deriving (Show)
