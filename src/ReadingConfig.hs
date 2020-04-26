{-# LANGUAGE OverloadedStrings #-}

module ReadingConfig where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B

data TestConfig = TestConfig { a :: String, b :: Int }

defaultConfig :: TestConfig
defaultConfig = TestConfig { a = "aaa", b = 333 }

readConfig :: IO ()
readConfig = do
  file <- B.readFile "testconfig.json"
  let config = extractConfig file
  putStrLn $ show $ a config
  putStrLn $ show $ b config
  return ()

extractConfig :: B.ByteString -> TestConfig
extractConfig file = case decode file of
  Nothing -> error "cannot decode file"
  Just val -> case (parseMaybe parseTestConfig val) of
          Nothing -> error "could not extract config values"
          Just config -> config

parseTestConfig :: Value -> Parser TestConfig
parseTestConfig (Object o) = do
  aValue <- o .: "a"
  bValue <- o .: "b"
  return $ TestConfig { a = aValue, b = bValue }
