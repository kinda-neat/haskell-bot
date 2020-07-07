module Main where

import Bot.MachineCode (Bot(..), runBot)
import ReadConfig (readConfig)
import System.Environment (getArgs)
import UserPreferences (createPrefsFileIfDontExist)

main :: IO ()
main = do
  (fileName:_) <- getArgs
  config <- readConfig fileName
  createPrefsFileIfDontExist
  runBot config Telegram
