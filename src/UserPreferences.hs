{-# LANGUAGE OverloadedStrings #-}

module UserPreferences where

import Control.Exception
import Control.Monad (guard)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import System.IO

instance FromJSON UserPreferences where
  parseJSON (Object o) = UserPreferences <$> o .: "selectedOption"

instance ToJSON UserPreferences where
  toJSON (UserPreferences {selectedOption = selectedOption}) =
    object ["selectedOption" .= selectedOption]

type SelectedOption = Int

type UserId = String

type Preferences = HM.HashMap String UserPreferences

data UserPreferences = UserPreferences
  { selectedOption :: SelectedOption
  } deriving (Show)

userPreferencesFileName :: String
userPreferencesFileName = "user-preferences.json"

ensureUserPreferencesFileExists :: IO ()
ensureUserPreferencesFileExists = do
  fileExist <- doesFileExist userPreferencesFileName
  if not fileExist
    then writeFile userPreferencesFileName "{}"
    else return ()

saveSelectedOption :: UserId -> SelectedOption -> IO ()
saveSelectedOption userId option = do
  preferences <- readUserPreferences
  writeUserPreferences
    (HM.insert userId (UserPreferences {selectedOption = option}) preferences)

-- about laziness and strictness https://stackoverflow.com/questions/2527271/in-haskell-i-want-to-read-a-file-and-then-write-to-it-do-i-need-strictness-ann
readUserPreferences :: IO Preferences
readUserPreferences =
  withFile
    userPreferencesFileName
    ReadMode
    (\handle -> do
       file <- BS.hGetContents handle
       let eitherConfig = eitherDecodeStrict file
           preferences =
             case eitherConfig of
               Left e -> error e
               Right val -> parsePreferences val
       return preferences)

getUserSpecifiedOption :: UserId -> IO (Maybe Int)
getUserSpecifiedOption userId = do
  prefs <- readUserPreferences
  return $ selectedOption <$> HM.lookup userId prefs

writeUserPreferences :: Preferences -> IO ()
writeUserPreferences prefs =
  withFile userPreferencesFileName WriteMode (\h -> B.hPut h (encode prefs))

parsePreferences :: Value -> Preferences
parsePreferences val =
  case parseMaybe parseJSON val of
    Nothing -> error "could not extract config values"
    Just config -> config
