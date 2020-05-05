module Bot where

data BotCommand
  = Help ChatId
  | Repeat ChatId
           UserId
  | Message ChatId
            UserId
            String
  | SelectOption QuestionId
                 UserId
                 (Maybe ReplyTimes)
  deriving (Show)

data BotActions = BotActions
  { runBot :: BotPayload -> IO (BotCommand, BotPayload)
  , showBotDescription :: ChatId -> String -> IO ()
  , askNumberToRepeatMessage :: Integer -> [Option] -> String -> Option -> IO ()
  , confirmSelectedOptionSaved :: QuestionId -> String -> IO ()
  , replyToMessage :: ChatId -> ReplyTimes -> String -> IO ()
  }

data UserPrefsActions = UserPrefsActions
  { upSaveSelectedOption :: Integer -> Int -> IO ()
  , upGetUserSpecifiedOption :: UserId -> IO (Maybe Int)
  }

type LastUpdateId = Maybe Integer

type UserId = Integer

type ChatId = Integer

type Option = Int

type ReplyTimes = Int

type QuestionId = String

data BotPayload
  = TelegramRunBotPayload LastUpdateId
  | TelegramShowDescPayload Integer
  | VKBotPayload
  deriving (Show)
