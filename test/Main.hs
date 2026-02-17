module Main where

import Test.Hspec
import qualified Test.Nostr.ClientSpec
import qualified Test.Nostr.Nip19Spec

main :: IO ()
main = hspec $ do
  Test.Nostr.ClientSpec.spec
  Test.Nostr.Nip19Spec.spec
