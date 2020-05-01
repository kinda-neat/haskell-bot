module Main where

import ReadConfig (Config(..), readConfig)
import System.Environment
import TelegramAPI

main :: IO ()
main = do
  (fileName:_) <- getArgs
  config <- readConfig fileName
  pollServer (telegram config) Nothing
  return ()
