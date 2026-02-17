{-# LANGUAGE OverloadedStrings #-}

module Test.Nostr.ClientSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Nostr.Client
import Nostr.Event
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "EventBuilder" $ do
    it "creates a Short Text Note (Kind 1)" $ do
      let content = "Hello World"
      let builder = shortNote content
      ebKind builder `shouldBe` 1
      ebContent builder `shouldBe` content
      ebTags builder `shouldBe` []

    it "adds tags correctly with withTag" $ do
      let builder = shortNote "test" & withTag ["t", "haskell"]
      ebTags builder `shouldBe` [["t", "haskell"]]

    it "overrides kind with withKind" $ do
      let builder = shortNote "test" & withKind 42
      ebKind builder `shouldBe` 42

    it "adds reply tag with withReply" $ do
      let eid = "0000000000000000000000000000000000000000000000000000000000000001"
      let builder = shortNote "reply" & withReply eid
      ebTags builder `shouldBe` [["e", eid]]

    it "adds mention tag with withMention" $ do
      let pubkey = "0000000000000000000000000000000000000000000000000000000000000002"
      let pk = case mkPubKey pubkey of Right p -> p; Left _ -> error "Invalid pubkey"
      let builder = shortNote "mention" & withMention pk
      ebTags builder `shouldBe` [["p", pubkey]]
