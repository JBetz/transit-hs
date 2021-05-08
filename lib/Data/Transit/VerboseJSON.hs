{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Transit.VerboseJSON where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Data.Transit
import qualified Data.Vector as V

import Data.Transit.JSON

toTransitVerboseJSON :: ToTransit a => a -> JSON
toTransitVerboseJSON = A.toJSON . VerboseValue . toTransit

encodeTransitVerboseJSON :: ToTransit a => a -> ByteString
encodeTransitVerboseJSON = A.encode . toTransitVerboseJSON

instance ToJSON VerboseValue where
  toJSON (VerboseValue transit) =
    run . evalState emptyWriteCache $ writeValue transit

    where
      writeValue v = write (AsValue, v)

      write :: (WriteMode, Value) -> Eff (State WriteCache ': effs) JSON
      write = \case

        (mode, PointInTime time) -> writeTaggedValue (mode == AsKey) 't' $ T.pack $ show $ round (1000 * utcTimeToPOSIXSeconds time)


        (_, List vals) -> do
          jsonVals <- traverse writeValue vals
          pure $ A.Object $ HashMap.singleton "~#list" (A.Array $ V.fromList jsonVals)

        (_, Set vals) -> do
          jsonVals <- traverse writeValue (Set.toList vals)
          pure $ A.Object $ HashMap.singleton "~#set" (A.Array $ V.fromList jsonVals)

        (_, Map map) -> do
          jsonVals <- traverse (\(k, v) -> do
            let textK = writeKey k
            jsonV <- writeValue v
            pure (textK, jsonV)) (M.assocs map)
          pure $ A.Object $ HashMap.fromList jsonVals

        (mode, TaggedValue tag val) -> writeTaggedValue (mode == AsKey) tag val

        (_, v) ->
          pure $ toJSON v

      writeKey :: Value -> Text
      writeKey _str = undefined

      writeTaggedValue :: Bool -> Char -> Text -> Eff (State WriteCache ': effs) JSON
      writeTaggedValue cacheable tag val =
        let output = T.pack $ '~':tag:T.unpack val
        in fmap A.String $
          if cacheable
            then cacheWrite output
            else pure output
