{-# LANGUAGE OverloadedStrings #-}

module RedishSpec where 

import Test.Hspec
import Redish
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.ByteString.Char8 (ByteString, pack)

parseReplySpec :: Spec
parseReplySpec = do
  describe "parseReply" $ do
    let emptyReply = Bulk Nothing
    let nb s = Bulk (Just (pack s))
    let unknownReply = nb "unknown"
    let getReply = nb "get"
    let setReply = nb "set"

    let quickCheck s = (\s -> length (take 5 s) < 5)


    it "returns nothing with single bulk" $ do
      parseReply emptyReply `shouldBe` (Nothing)
    
    it "unknown command in multibulk returns unknown" $ do
      parseReply (MultiBulk (Just [unknownReply])) `shouldBe` (Just Unknown)

    prop "get command in multibulk returns get a" $ \n -> do
      parseReply (MultiBulk (Just [getReply, nb n])) `shouldBe` (Just (Get (pack n)))

    prop "set command in multibulk returns set a" $ \(n,v) -> do
      parseReply (MultiBulk (Just [setReply, nb n, nb v])) `shouldBe` (Just (Set (pack n) (pack v)))

spec :: Spec
spec = do
  parseReplySpec