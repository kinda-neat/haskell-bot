-- https://artyom.me/aeson

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module AesonPractice where

import Data.Aeson
import Data.Aeson.Types
import GHC.Exts
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T
import Data.Maybe
import qualified Data.Vector as V
import Control.Monad as CM
import Control.Applicative
import Data.Foldable
import Control.Monad (when)
import Text.Read (readMaybe)

-- val :: Value
-- val = Object $ fromList [
--   ("numbers", Array $ fromList [Number 1, Number 2, Number 3])
--   , ("boolean", Bool True)
--   ]

val :: Value 
val = object [
  "boolean" .= True,
  "numbers" .= [1,2,3::Int]
  ]

revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array x) = Array (fmap revStrings x)
revStrings (Object x) = let revPair (k,v) = (T.reverse k, revStrings v)
                        in Object . fromList . map revPair . HM.toList $ x
revStrings other = other

revJSON = encode . revStrings . fromJust . decode

-- parseTuple :: Value -> Parser (String, Bool)
-- parseTuple (Object obj) = do
--   let mbFieldA = HM.lookup "a" obj

--   fieldA <- case mbFieldA of
--     Just x -> return x
--     Nothing -> fail "no field 'a'"
  
--   -- a <- case fieldA of
--   --   String x -> return (T.unpack x)
--   --   _ -> fail "expected a string"

--   a <- withText "string" $ \x -> return (T.unpack x)

--   b <- case HM.lookup "b" obj of
--     Just (Bool x) -> return x
--     Just _ -> fail "expected a boolean"
--     Nothing -> fail "no field 'b'"

--   return (a,b)

-- parseArray :: Value -> Parser [(String, Bool)]
-- parseArray (Array arr) = CM.mapM parseTuple (V.toList arr)
-- parseArray _ = fail "expected an array"

-- withArray :: String -> (Array -> Parser a) -> Value -> Parser a
-- withArray expected f (Array arr) = f arr
-- withArray expected f value = fail "expected ..."

parseArray :: Value -> Parser [(String, Bool)]
parseArray = withArray "array of tuples" $ \arr ->
              mapM parseTuple (V.toList arr)

parseStrings :: Value -> Parser String
parseStrings = withText "string" $ \x -> return (T.unpack x)

-- class FromJSON a where
--   parseJSON :: Value -> Parser a

-- instance FromJSON String where
--   parseJSON = withText "String" (\x -> return (T.unpack x))

-- instance FromJSON Bool where
--   parseJSON = withText "Bool" return

-- instance FromJSON a => FromJSON [a] where
--   parseJSON = withArray "[a]" $ mapM parseJSON . V.toList

-- parseTuple :: Value -> Parser (String, Bool)
-- parseTuple = withObject "tuple" $ \obj -> do
--   a <- case HM.lookup "a" obj of
--     Just x -> parseJSON x
--     Nothing -> fail "no field 'a'"
  
--   b <- case HM.lookup "b" obj of
--     Just x -> parseJSON x
--     Nothing -> fail "no field 'b'"

--   return (a,b)

-- (.:) :: (FromJSON a) => Object -> Text -> Parser a
-- o .: key = case HM.lookup key o of
--             Nothing -> fail ("key " ++ show key ++ " not present")
--             Just v -> parseJSON v

parseTuple = withObject "tuple" $ \o -> do
  a <- o .: "a"
  b <- o .: "b"
  return (a, b)

-- parseTuple = withObject "tuple" $ \o -> (,) <$> o .: "a" <*> o .: "b" -- (,) is a constructor

data Person = Person { name :: String, age :: Int }

instance FromJSON Person where
  parseJSON = withObject "person" $ \o ->
    Person <$> o .: "name" <*> o.: "age"

instance ToJSON Person where
  toJSON p = object [
    "name" .= name p,
    "age" .= age p ]

-- instance FromJSON Person where
--   parseJSON = withObject "person" $ \o -> do
--     name <- o .: "name"
--     age <- o .: "age"
--     return Person{..}

-- instance ToJSON Person where
--   toJSON Person{..} = object [
--     "name" .= name,
--     "age" .= age ]

-- instance FromJSON Person where
--   parseJSON = withObject "person" $ \o -> do
--     name <- o .: "name"
--     age <- o .:? "age" .! = 18
--     -- age <- optional (o .: "age")
--     return Person{..}

-- instance FromJSON Person where
--   parseJSON = withObject "person" $ \o -> do
--     name <- o .: "name"
--     age <- o .: "age" <|> o .: "AGE"
--     return Person{..}

-- instance FromJSON Person where
--   parseJSON = withObject "person" $ \o -> do
--     name <- o .: "name"
--     age <- asum [
--       o .: "age"
--       , do
--           s <- o .: "age"
--           case readMaybe s of
--             Nothing -> fail "not a number"
--             Just x -> return x
--       , fst <$> o .: "AGE"
--       , do
--           CM.guard (name == "John") return 24
--       ]
--     return Person{..}

data Something
  = PersonM { personMName :: String, personMAge :: Int }
  | Book { bookName :: String, author :: String }

instance FromJSON Something where
  parseJSON = withObject "book or person" $ \o -> asum [
    PersonM <$> o .: "name" <*> o .: "age",
    Book <$> o .: "bookName" <*> o .: "author"
    ]

-- instance FromJSON Something where
--   parseJSON = withObject "book or person" $ \o -> do
--     kind <- o .: "kind"
--     case kind of
--       "person" -> Person <$> o .: "name" <*> o .: "age"
--       "book" -> Book <$> o .: "bookName" <*> o .: "author"
--       _ -> fail ("unknown kind: " ++ kind)

-- data Name = Name {
--   name :: String,
--   surname :: String
--   }

-- data RussianName = RussianName {
--   russianName :: Name,
--   russianPatronymic :: String
--   }

-- instance FromJSON Name where
--   parseJSON = withObject "name" $ \o -> do
--     name <- o .: "name"
--     surname <- o .: "surname"
--     return Name{..}

-- instance ToJSON Name where
--   toJSON Name{..} = object [
--     "name" .= name,
--     "surname" .= surname
--     ]

-- instance FromJSON RussianName where
--   parseJSON = withObject "name" $ \o -> do
--     russianName <- parseJSON (Object o)
--     russianPatronymic <- o .: "patronymic"
--     return RussianName{..}
  
-- instance ToJSON RussianName where
--   toJSON RussianName{..} = Object $
--     toObject russianName <>
--     fromList ["patronymic" .= russianPatronymic]

-- toObject :: ToJSON a => a -> Object
-- toObject a = case toJSON a of
--   Object o -> o
--   _ -> error "toObject: value isn't an Object"

data Referer = Referer {
  domain :: String,
  pathAccesses :: [(String, Int)]
  } deriving (Show)

parseReferers :: Value -> Parser [Referer]
parseReferers p =
  map (\(domain, accesses) -> Referer domain (HM.toList accesses)) .
  HM.toList <$>
  parseJSON p

-- parseReferers :: Value -> Parser [Referer]
-- parseReferers =
--   withObject "referers" $ \o ->
--     for (HM.toList o) $ \(domain, referer) -> do
--       accesses <- HM.toList <$> parseJSON referer
--       let accesses' = map (\(page, n) -> (T.unpack page, n)) accesses
--       return $ Referer {
--         domain = T.unpack domain,
--         pathAccesses = accesses'
--         }

