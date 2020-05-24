{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}

module Data.Transit where

import Control.Monad.Freer
import Control.Monad.Freer.State
import qualified Control.Monad.Freer.Error as E
import Control.DeepSeq
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.ByteString
import Data.Char (chr, ord)
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time
import Data.Time.Clock.POSIX
import Data.UUID
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof, vectorOf)

data Result a
  = Success a
  | Error String
  deriving (Eq, Show, Generic, NFData)

data Value
  = Null
  | String Text
  | Boolean Bool
  | Integer Int64
  | Float Float
  | Bytes ByteString
  | Keyword Text
  | Symbol Text
  | PointInTime UTCTime
  | UUID UUID
  | Char Char
  | Array (Vector Value)
  | List [Value]
  | Set (Set Value)
  | Map (Map Value Value)
  | TaggedValue Char Text
  deriving (Eq, Ord, Show, Generic, NFData)

instance Arbitrary Value where
  arbitrary = oneof
    [ pure Null
    , String . T.pack <$> arbitrary
    , Boolean <$> arbitrary
    , Integer <$> arbitrary
    , Float <$> arbitrary
    , Keyword . T.pack <$> arbitrary
    , Symbol . T.pack <$> arbitrary
    , PointInTime . posixSecondsToUTCTime . fromInteger <$> arbitrary
    , Array . V.fromList <$> vectorOf 3 arbitrary
    , List <$> vectorOf 3 arbitrary
    , Set . Set.fromList <$> vectorOf 3 arbitrary
    , Map . M.fromList <$> vectorOf 3 arbitrary
    , TaggedValue <$> arbitrary <*> fmap T.pack arbitrary
    ]

data ArrayTag
  = ArrayTag_Quote
  | ArrayTag_Map
  | ArrayTag_List
  | ArrayTag_Set
  | ArrayTag_Unknown String
  deriving (Eq, Ord, Show)

class ToTransit a where
  toTransit :: a -> Value

typeOf :: Value -> String
typeOf = \case
  Null -> "Null"
  String _ -> "String"
  Boolean _ -> "Boolean"
  Integer _ -> "Integer"
  Float _ -> "Float"
  Bytes _ -> "Bytes"
  Keyword _ -> "Keyword"
  Symbol _ -> "Symbol"
  PointInTime _ -> "PointInTime"
  UUID _ -> "UUID"
  Char _ -> "Char"
  Array _ -> "Array"
  List _ -> "List"
  Set _ -> "Set"
  Map _ -> "Map"
  TaggedValue _ _ -> "TaggedValue"

type Parser a = Eff '[State ReadCache, E.Error String] a

runParser :: Parser a -> Either String a
runParser = run . E.runError . evalState emptyReadCache

typeMismatch :: String -> Value -> Parser a
typeMismatch expected actual =
  E.throwError $ "expected " ++ expected ++ ", but encountered " ++ typeOf actual

valueMismatch :: String -> String -> Parser a
valueMismatch expected actual =
  E.throwError $ "expected " ++ expected ++ ", but encountered " ++ actual

mapGetKeyword :: FromTransit v => Text -> Map Value Value -> Parser v
mapGetKeyword keyword map =
  case M.lookup (Keyword keyword) map of
    Just val -> fromTransit val
    Nothing -> valueMismatch ("Map with key = " <> T.unpack keyword) $ show map

vecGet :: FromTransit v => Int -> Vector Value -> Parser v
vecGet index vector =
  case vector V.!? index of
    Just val -> fromTransit val
    Nothing -> valueMismatch ("Vector with index = " <> show index) $ show vector

withVec :: (Vector Value -> Parser a) -> Value -> Parser a
withVec parser (Array vector) = parser vector
withVec _ val = typeMismatch "Array" val

class FromTransit a where
  fromTransit :: Value -> Parser a

instance ToTransit Value where
  toTransit = id

instance FromTransit Value where
  fromTransit = pure

instance ToTransit () where
  toTransit () = Null

instance FromTransit () where
  fromTransit Null = pure ()
  fromTransit val = typeMismatch "Null" val

instance ToTransit a => ToTransit (Maybe a)  where
  toTransit (Just x) = toTransit x
  toTransit Nothing = Null

instance FromTransit a => FromTransit (Maybe a) where
  fromTransit Null = pure Nothing
  fromTransit x = Just <$> fromTransit x

instance ToTransit String where
  toTransit = String . T.pack

instance FromTransit String where
  fromTransit (String x) = pure $ T.unpack x
  fromTransit val = typeMismatch "String" val

instance ToTransit Text where
  toTransit = String

instance FromTransit Text where
  fromTransit (String x) = pure x
  fromTransit val = typeMismatch "String" val

instance ToTransit Bool where
  toTransit = Boolean

instance FromTransit Bool where
  fromTransit (Boolean x) = pure x
  fromTransit val = typeMismatch "Boolean" val

instance ToTransit Int where
  toTransit = Integer . fromIntegral

instance FromTransit Int where
  fromTransit (Integer x) = pure $ fromIntegral x
  fromTransit val = typeMismatch "Integer" val

instance ToTransit Int64 where
  toTransit = Integer

instance FromTransit Int64 where
  fromTransit (Integer x) = pure x
  fromTransit val = typeMismatch "Integer" val

instance ToTransit a => ToTransit [a] where
  toTransit list = List $ toTransit <$> list

instance FromTransit a => FromTransit [a] where
  fromTransit (List list) = traverse fromTransit list
  fromTransit val = typeMismatch "List" val

instance ToTransit a => ToTransit (Vector a) where
  toTransit list = Array $ toTransit <$> list

instance FromTransit a => FromTransit (Vector a) where
  fromTransit (Array list) = traverse fromTransit list
  fromTransit val = typeMismatch "Array" val

instance (Eq a, ToTransit a) => ToTransit (HashSet a) where
  toTransit hashSet = Set $ Set.fromList $ toTransit <$> HashSet.toList hashSet

instance (Eq a, Hashable a, FromTransit a) => FromTransit (HashSet a) where
  fromTransit (Set set) = HashSet.fromList <$> traverse fromTransit (Set.elems set)
  fromTransit val = typeMismatch "Set" val

instance ToTransit UTCTime where
  toTransit = PointInTime

instance FromTransit UTCTime where
  fromTransit (PointInTime time) = pure time
  fromTransit val = typeMismatch "PointInTime" val

instance (ToTransit a, ToTransit b) => ToTransit (a, b) where
  toTransit (a, b) = Array $ V.fromList [toTransit a, toTransit b]

instance (FromTransit a, FromTransit b) => FromTransit (a, b) where
  fromTransit (Array arr) =  (,) <$> vecGet 0 arr <*> vecGet 1 arr
  fromTransit val = typeMismatch "Array" val

-- READER
type ReadCache = (Int, Bimap Int (Either ArrayTag Value))

emptyReadCache :: ReadCache
emptyReadCache = (0, Bimap.empty)

cacheRead :: Either ArrayTag Value -> Parser ()
cacheRead str = modify @ReadCache $ \(counter, cache) -> if
  | Bimap.memberR str cache -> (counter, cache)
  | counter == maxCounter -> (1, Bimap.singleton 0 str)
  | otherwise -> (counter + 1, Bimap.insert counter str cache)

lookupCode :: Text -> Parser (Either ArrayTag Value)
lookupCode code = do
  cache <- gets @ReadCache snd
  case Bimap.lookup (codeToIndex code) cache of
    Just val -> pure val
    Nothing -> E.throwError $ "Invalid cache code: " <> T.unpack code

-- WRITER
type WriteCache = (Int, Map Text Text)

emptyWriteCache :: WriteCache
emptyWriteCache = (0, mempty)

cacheWrite :: Text -> Eff (State WriteCache ': effs) Text
cacheWrite str =
  if T.length str > 3
    then do
      cache <- gets @WriteCache snd
      case M.lookup str cache of
        Just code ->
          pure code
        Nothing -> do
          modify @WriteCache $ \(counter, cache) ->
            if counter == maxCounter
              then emptyWriteCache
              else (counter + 1, M.insert str (indexToCode counter) cache)
          pure str
    else
      pure str

-- ENCODING
cacheCodeDigits, baseCharIndex, maxCounter :: Int
cacheCodeDigits = 44
baseCharIndex = 48
maxCounter = cacheCodeDigits ^ 2

indexToCode :: Int -> Text
indexToCode i =
  let hi = i `div` cacheCodeDigits
      lo = i `mod` cacheCodeDigits
  in T.cons cacheCodePrefix $ T.pack $
    if hi == 0
      then [chr (lo + baseCharIndex)]
      else [chr (hi + baseCharIndex), chr (lo + baseCharIndex)]

codeToIndex :: Text -> Int
codeToIndex code =
  if T.length code == 2
    then ord (T.index code 1) - baseCharIndex
    else ((ord (T.index code 1) - baseCharIndex) * cacheCodeDigits) + (ord (T.index code 2) - baseCharIndex)

cacheCodePrefix :: Char
cacheCodePrefix = '^'

isCacheable :: Text -> Bool
isCacheable str =
  T.length str > 3 &&
    ("~#" `T.isPrefixOf` str ||
     "~:" `T.isPrefixOf` str ||
     "~$" `T.isPrefixOf` str)

isCacheCode :: Text -> Bool
isCacheCode str =
  not (T.null str) && T.head str == '^' && str /= "^ "