{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Success)

import Data.Transit
import Data.Transit.JSON (fromTransitJSON, toTransitJSON)

main :: IO ()
main =
  hspec spec

spec :: Spec
spec = do
  describe "transit" $ do
    prop "cache code calculation is isomorphic" $ \i ->
      (i > 0 && i < maxCounter) ==> codeToIndex (indexToCode i) == i

    {--
    prop "transit serialization is isomorphic" $ \(v :: Value) ->
      (fromTransitJSON . toTransitJSON $ v) == Success v
    --}