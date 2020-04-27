module Main where

import System.Environment
import ReadConfig (readConfig, Config(..))

main :: IO ()
main = do
  (fileName:_) <- getArgs
  config <- readConfig fileName
  putStrLn $ show config
  return ()
