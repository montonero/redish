{-# LANGUAGE OverloadedStrings #-}

module RedishSpec where 

import Test.Hspec
import Redish
import Data.ByteString.Char8 (ByteString, pack)

parseReplySpec :: Spec
parseReplySpec = do
  describe "parseReply" $ do
    let emptyReply = Bulk Nothing
    let unknownReply = Bulk (Just "unknown")
    let getReply = Bulk (Just "get")
    let setReply = Bulk (Just "set")
    let name = Bulk (Just "name")
    let value = Bulk (Just "woz")

    it "returns nothing with single bulk" $ do
      parseReply emptyReply `shouldBe` (Nothing)
    
    it "unknown command in multibulk returns unknown" $ do
      parseReply (MultiBulk (Just [unknownReply])) `shouldBe` (Just Unknown)

    it "get command in multibulk returns get a" $ do
      parseReply (MultiBulk (Just [getReply, name])) `shouldBe` (Just (Get "name"))

    it "set command in multibulk returns set a" $ do
      parseReply (MultiBulk (Just [setReply, name, value])) `shouldBe` (Just (Set "name" "woz"))

spec :: Spec
spec = do
  parseReplySpec