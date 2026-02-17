{-# LANGUAGE OverloadedStrings #-}

module Test.Nostr.Nip19Spec (spec) where

import Test.Hspec
import Test.QuickCheck
import Nostr.Nip19
import Nostr.Event (PubKey(..), EventId(..), mkPubKey, mkEventId)
import Nostr.Crypto (generateKeyPair, exportPubKey, exportSecKey, SecKey)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isJust, fromJust)

spec :: Spec
spec = do
  describe "NIP-19 Encoding/Decoding" $ do
    it "encodes and decodes PubKey (npub)" $ do
       -- Using a fixed key for reproducibility or property test
       -- Let's do a simple property check if valid hex
       let hex = "3bf0c63fcb9347d40134708a0d773553e299b71858752d508823f399f603d2b3"
       let pk = case mkPubKey hex of Right k -> k; Left _ -> error "Invalid pubkey"
       
       let encoded = toNpub pk
       encoded `shouldSatisfy` isJust
       
       let decoded = decode (fromJust encoded)
       decoded `shouldBe` Right (Nip19Pub pk)

    it "encodes and decodes EventId (note)" $ do
       let hex = "46f3c7bb33cc3019049b76dc89dbb96e34c247bdda68b6ad8632682793ff8a1a"
       let eid = case mkEventId hex of Right e -> e; Left _ -> error "Invalid event id"
       
       let encoded = toNote eid
       encoded `shouldSatisfy` isJust
       
       let decoded = decode (fromJust encoded)
       decoded `shouldBe` Right (Nip19Note eid)
