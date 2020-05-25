{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Transit.JSON where

import Control.Monad (join, when)
import Control.Monad.Freer
import qualified Control.Monad.Freer.Error as E
import Control.Monad.Freer.State
import Data.Aeson (FromJSON (..), ToJSON (..))
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Base64 as Base64
import Data.Either (rights)
import qualified Data.HashMap.Strict as HashMap
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Scientific
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Transit
import Data.Traversable (for)
import qualified Data.UUID as UUID
import qualified Data.Vector as V
import qualified Network.URI as Network
import Text.Read (readMaybe)

toTransitJSON :: ToTransit a => a -> A.Value
toTransitJSON = A.toJSON . toTransit

fromTransitJSON :: FromTransit a => A.Value -> Result a
fromTransitJSON json =
  case A.fromJSON json of
    A.Success r ->
      case runParser (fromTransit r) of
        Right value -> Success value
        Left err -> Error err
    A.Error val -> Error val

encodeTransitJSON :: ToTransit a => a -> ByteString
encodeTransitJSON = A.encode . toTransitJSON

decodeTransitJSON :: FromTransit a => ByteString -> Result a
decodeTransitJSON bs =
  case A.decode bs of
    Just res -> fromTransitJSON res
    Nothing -> Error "Invalid JSON"

instance A.FromJSON Value where
  parseJSON json =
    case runParser $ parse json of
      Right (Right value) -> pure value
      Right (Left arrayTag) -> fail $ "Invalid transit data. Found array tag at root: " <> show arrayTag
      Left parseError -> fail parseError

    where

      parse :: A.Value -> Parser (Either ArrayTag Value)
      parse = \case
        A.Array elems -> do
          parsedElems <- traverse parse elems
          case V.toList parsedElems of
            Left arrayTag:tail ->
              pure $ Right $ decodeArray arrayTag $ rights tail
            arrayElems ->
              pure $ Right $ Array $ V.fromList $ rights arrayElems
        A.String str ->
          parseString str
        A.Null -> pure $ Right Null
        A.Bool bool -> pure $ Right $ Boolean bool
        A.Number num -> pure $ Right $
          case toBoundedInteger num of
            Just int -> Integer64 int
            Nothing -> Float (toRealFloat num)
        A.Object obj -> do
          assocs <- traverse (\(k, v) -> do
            parsedKey <- parse $ A.String k
            parsedValue <- parse v
            pure (parsedKey, parsedValue)) (HashMap.toList obj)
          case assocs of
            [(Left arrayTag, Right (Array elems))] ->
              pure $ Right $ decodeArray arrayTag $ V.toList elems
            _ -> do
              assocsValues <- for assocs $ \case
                (Right k, Right v) -> pure (k, v)
                (k, v) -> E.throwError $ "Invalid map association: " <> show (k, v)
              pure $ Right $ Map $ M.fromList assocsValues

      parseString :: Text -> Parser (Either ArrayTag Value)
      parseString str =
        if isCacheCode str
          then lookupCode str
          else do
            val <- case T.unpack str of
              "^ " ->
                pure $ Left ArrayTag_Map
              '~':'#':tail ->
                pure $ case tail of
                  "set" -> Left ArrayTag_Set
                  "list" -> Left ArrayTag_List
                  "'" -> Left ArrayTag_Quote
                  _ -> mkString tail
              '~':'~':tail ->
                pure $ mkString $ '~':tail
              '~':'^':tail ->
                pure $ mkString $ '^':tail
              '~':'`':tail ->
                pure $ mkString $ '`':tail
              '~':decodeTag:tail -> do
                decoded <- decodeString decodeTag $ T.pack tail
                pure $ Right decoded
              _ ->
                pure $ Right $ String str
            when (isCacheable str) $ cacheRead val
            pure val

        where
          mkString = Right . String . T.pack

      decodeString :: Char -> (Text -> Parser Value)
      decodeString = \case
        '_' -> \case
          "" -> pure Null
          val -> E.throwError $ T.unpack val
        '?' -> \case
          "t" -> pure $ Boolean True
          "f" -> pure $ Boolean False
          val -> valueMismatch "'t' or 'f'" (T.unpack val)
        'i' -> parseText readText Integer64 "Integer64"
        'n' -> parseText readText Integer "Integer"
        'd' -> parseText readText Float "Float"
        'f' -> parseText readText Decimal "Decimal"
        'b' -> parseTextEither (Base64.decode . T.encodeUtf8) Bytes "Bytes"
        ':' -> pure . Keyword
        '$' -> pure . Symbol
        'm' -> parseText readText (PointInTime . posixSecondsToUTCTime . fromIntegral . (`div` 1000)) "POSIXSeconds"
        't' -> parseText (parseTimeM True defaultTimeLocale "" . T.unpack) PointInTime "Timestamp"
        'u' -> parseText UUID.fromText UUID "UUID"
        'r' -> parseText (Network.parseURI . T.unpack) URI "URI"
        'c' -> pure . Char . T.head
        unknownChar -> pure . TaggedValue unknownChar

        where
          parseText :: (Text -> Maybe a) -> (a -> Value) -> String -> Text -> Parser Value
          parseText parser constructor typeName txt =
            case parser txt of
              Just val -> pure $ constructor val
              Nothing -> E.throwError $ "Invalid " <> typeName <> ": " <> T.unpack txt

          parseTextEither :: (Text -> Either String a) -> (a -> Value) -> String -> Text -> Parser Value
          parseTextEither parser constructor typeName txt =
            case parser txt of
              Right val -> pure $ constructor val
              Left err -> E.throwError $ "Invalid " <> typeName <> ": " <> err

          readText :: Read a => Text -> Maybe a
          readText = readMaybe . T.unpack

      decodeArray :: ArrayTag -> [Value] -> Value
      decodeArray = \case
        ArrayTag_Map -> \case
          elems -> Map (buildMap elems)
        ArrayTag_Set -> \case
          [Array elems] -> Set $ Set.fromList $ V.toList elems
          _ -> error "Invalid Set"
        ArrayTag_List -> \case
          [Array elems] -> List $ V.toList elems
          _ -> error "Invalid List"
        ArrayTag_Quote -> \case
          [String str] -> String str
          _ -> error "Invalid Quoteh"
        ArrayTag_Unknown tag ->
          error $ "Unknown tag: " <> tag


      buildMap :: [Value] -> Map Value Value
      buildMap elems =
        M.fromList $ (\[a, b] -> (a, b)) <$> chunksOf 2 elems

data WriteMode
  = AsKey
  | AsValue
  deriving (Eq)

instance ToJSON Value where
  toJSON transit =
    run . evalState emptyWriteCache $ writeValue transit

    where
      writeKey v = write (AsKey, v)
      writeValue v = write (AsValue, v)

      write :: (WriteMode, Value) -> Eff (State WriteCache ': effs) A.Value
      write = \case
        (AsKey, Null) -> writeTaggedValue False '_' ""
        (AsValue, Null) -> pure A.Null

        (_, String str) -> pure $ writeString str

        (AsKey, Integer64 int) -> writeTaggedValue True 'i' $ T.pack $ show int
        (AsValue, Integer64 int) -> pure $ A.Number $ fromIntegral int

        (mode, Integer int) -> writeTaggedValue (mode == AsKey) 'n' $ T.pack $ show int

        (AsKey, Float float) -> writeTaggedValue True 'd' $ T.pack $ show float
        (AsValue, Float float) -> pure $ A.Number $ fromFloatDigits float

        (AsKey, Decimal scientific) -> writeTaggedValue True 'f' $ T.pack $ show scientific
        (AsValue, Decimal scientific) -> pure $ A.Number scientific

        (AsKey, Boolean bool) -> writeTaggedValue True '?' $ if bool then "t" else "f"
        (AsValue, Boolean bool) -> pure $ A.Bool bool

        (AsKey, Bytes bytes) -> writeTaggedValue True 'b' $ T.decodeUtf8 bytes
        (AsValue, Bytes bytes) -> pure $ A.String $ T.decodeUtf8 (Base64.encode bytes)

        (_, Keyword keyword) -> writeTaggedValue True ':' keyword

        (_, Symbol symbol) -> writeTaggedValue True '$' symbol

        (mode, PointInTime time) -> writeTaggedValue (mode == AsKey) 'm' $ T.pack $ show $ round (1000 * utcTimeToPOSIXSeconds time)

        (mode, UUID uuid) -> writeTaggedValue (mode == AsKey) 'u' $ UUID.toText uuid

        (mode, URI uri) -> writeTaggedValue (mode == AsKey) 'r' $ T.pack $ show uri

        (_, Char char) ->  writeTaggedValue False 'c' $ T.singleton char

        (_, Array vals) -> do
          jsonVals <- traverse writeValue vals
          pure $ A.Array jsonVals

        (_, List vals) -> do
          jsonTag <- cacheWrite "~#list"
          jsonVals <- traverse writeValue vals
          pure $ A.Array $ V.fromList [ A.String jsonTag, A.Array $ V.fromList jsonVals ]

        (_, Set vals) -> do
          jsonTag <- cacheWrite "~#set"
          jsonVals <- traverse writeValue (Set.toList vals)
          pure $ A.Array $ V.fromList [ A.String jsonTag, A.Array $ V.fromList jsonVals ]

        (_, Map map) -> do
          jsonVals <- traverse (\(k, v) -> do
            jsonK <- writeKey k
            jsonV <- writeValue v
            pure [jsonK, jsonV]) (M.assocs map)
          pure $ A.Array $ V.fromList $ "^ ":join jsonVals

        (mode, TaggedValue tag val) -> writeTaggedValue (mode == AsKey) tag val

      writeString :: Text -> A.Value
      writeString str = A.String $
        case T.unpack str of
          '~':_ -> T.cons '~' str
          '^':_ -> T.cons '~' str
          '`':_ -> T.cons '~' str
          _ -> str

      writeTaggedValue :: Bool -> Char -> Text -> Eff (State WriteCache ': effs) A.Value
      writeTaggedValue cacheable tag val =
        let output = T.pack $ '~':tag:T.unpack val
        in fmap A.String $
          if cacheable
            then cacheWrite output
            else pure output
