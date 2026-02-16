{-# LANGUAGE OverloadedStrings #-}

module Main where

import Nostr.Event
import Nostr.Crypto
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Word (Word64)

main :: IO ()
main = do
  putStrLn "=== Nostr NIP-01 Library Demo ==="
  putStrLn ""
  
  -- Generate a key pair
  putStrLn "Generating key pair..."
  (secKey, pubKey) <- generateKeyPair
  putStrLn $ "Public Key: " ++ T.unpack (unPubKey pubKey)
  putStrLn ""
  
  -- Get current timestamp
  timestamp <- fmap (floor . realToFrac) getPOSIXTime :: IO Word64
  
  -- Create an unsigned event (kind 1 = text note)
  let event = createUnsignedEvent 
        pubKey
        timestamp
        1  -- kind 1 = text note
        []  -- no tags
        "Hello from nostr.hs! This is a test event."
  
  putStrLn "Created unsigned event"
  putStrLn ""
  
  -- Sign the event
  putStrLn "Signing event..."
  signResult <- signEvent secKey event
  case signResult of
    Left err -> putStrLn $ "Error signing event: " ++ T.unpack err
    Right signedEvent -> do
      putStrLn "Event signed successfully!"
      putStrLn ""
      putStrLn "Event JSON:"
      BL.putStrLn $ encode signedEvent
      putStrLn ""
      
      -- Verify the signature
      putStrLn "Verifying signature..."
      isValid <- verifySignature signedEvent
      putStrLn $ "Signature valid: " ++ show isValid
