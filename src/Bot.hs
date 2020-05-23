module Bot where

data BotCommand
  = Help ChatId
  | Repeat ChatId
           UserId
  | Message ChatId
            UserId
            MessageText
  | SelectOption QuestionId
                 UserId
                 (Maybe Option)
  deriving (Show)

data BotActions = BotActions
  { runBot :: BotPayload -> IO (BotCommand, BotPayload)
  , showBotDescription :: ChatId -> BotDescription -> IO ()
  , askNumberToRepeatMessage :: ChatId -> Option -> IO ()
  , confirmSelectedOptionSaved :: QuestionId -> ConfirmText -> IO ()
  , replyToMessage :: ChatId -> Option -> MessageText -> IO ()
  }

data UserPrefsActions = UserPrefsActions
  { upSaveSelectedOption :: UserId -> Int -> IO ()
  , upGetUserSpecifiedOption :: UserId -> IO (Maybe Int)
  }

type LastUpdateId = Maybe Integer

newtype UserId =
  UserId Integer
  deriving (Show)

newtype ChatId =
  ChatId Integer
  deriving (Show)

type Option = Int

newtype QuestionId =
  QuestionId String
  deriving (Show)

type QuestionText = String

type ConfirmText = String

type BotDescription = String

type MessageText = String

data BotPayload
  = TelegramRunBotPayload LastUpdateId
  | TelegramShowDescPayload Integer
  | VKBotPayload { vkKey :: String
                 , vkServer :: String
                 , vkTs :: Int }
  deriving (Show)
