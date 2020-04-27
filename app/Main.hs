module Main where

import System.Environment
import ReadingConfig (readConfig, Config(..))

main :: IO ()
main = do
  (fileName:_) <- getArgs
  config <- readConfig fileName
  putStrLn $ show config
  return ()
