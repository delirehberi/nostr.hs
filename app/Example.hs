{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import qualified Data.Text as T

import Nostr.Client
import Nostr.Event
import Nostr.Relay
import Nostr.Crypto
import Nostr.Nip19
import Nostr.Nip05

main :: IO ()
main = do
  putStrLn "=== Nostr High-Level Client Example ==="
  
  -- 1. Generate Keys
  (secKey, pubKey) <- generateKeyPair
  let keys = Keys secKey pubKey (Just "wss://relay.damus.io") -- Example
  
  putStrLn $ "Generated Public Key (Hex): " ++ T.unpack (unPubKey pubKey)
  case toNpub pubKey of
    Just npub -> putStrLn $ "Generated Public Key (NIP-19): " ++ T.unpack npub
    Nothing -> putStrLn "Failed to encode npub"
    
  case toNsec secKey of
    Just nsec -> putStrLn $ "Generated Secret Key (NIP-19): " ++ T.unpack nsec
    Nothing -> putStrLn "Failed to encode nsec"
  
  -- 2. Define relays
  let relays = 
        [ "wss://relay.damus.io"
        , "wss://nos.lol" 
        -- Add more if needed, but these are reliable public ones
        ]
  
  -- 3. Connect (creates NostrEnv)
  putStrLn "Connecting to relays..."
  env <- connectRelays relays
  
  -- 4. Run App
  runNostrApp env $ do
    -- Publish a simple note (convenience function)
    let note = "Hello Nostr from Haskell Monad! 2026-02-16"
    liftIO $ putStrLn $ "Publishing note: " ++ show note
    publishShortNote keys note
    
    -- Publish using builder pattern with tags
    liftIO $ putStrLn "Publishing long-form article..."
    publish keys $ shortNote "My first long-form article from Haskell!"
      & withKind 30023
      & withAddressRef 30023 pubKey "haskell-nostr-intro"
      & withTag ["title", "Getting Started with Nostr in Haskell"]
      & withMention pubKey
    
    liftIO $ putStrLn "Wait 3 seconds for propagation..."
    liftIO $ threadDelay 3000000

    -- NIP-02: Follow a user
    liftIO $ putStrLn "Following Jack (founder)..."
    -- delirehberi's pubkey: npub1gmeu0wenescpjpymwmwgnkaedc6vy3aamf5tdtvxxf5z0yll3gdqatwl3v
    let jackPubkey = "82341f882b6eabcd2ba7f1ef90aad961cf074af15b9ef44a09f9d2a8fbf71d22"
    
    case parsePubKey jackPubkey of
      Just pk -> follow keys pk (Just "wss://relay.damus.io") (Just "jack")
      Nothing -> liftIO $ putStrLn "Invalid Jack pubkey"
    
    let delirehberiNpub = "npub1gmeu0wenescpjpymwmwgnkaedc6vy3aamf5tdtvxxf5z0yll3gdqatwl3v"
    case parsePubKey delirehberiNpub of
       Just pk -> do
         liftIO $ putStrLn $ "Following delirehberi (decoded from " ++ T.unpack delirehberiNpub ++ ")"
         
         -- Verify NIP-05
         let nip05 = "delirehberi@emre.xyz"
         liftIO $ putStrLn $ "Verifying NIP-05: " ++ T.unpack nip05 ++ "..."
         isVerified <- liftIO $ verifyNip05 nip05 pk
         if isVerified 
           then liftIO $ putStrLn "UNKNOWN: Verified NIP-05 ✅"
           else liftIO $ putStrLn "WARNING: NIP-05 Verification Failed ❌"
         
         follow keys pk (Just "wss://relay.damus.io") (Just "delirehberi@emre.xyz")
       Nothing -> liftIO $ putStrLn "Error: Not a public key"
    
    liftIO $ putStrLn "Wait 3 seconds..."
    liftIO $ threadDelay 3000000
    
    -- Fetch contacts
    contacts <- getContacts keys
    liftIO $ putStrLn $ "My Contacts: " ++ show contacts
    
    -- Query for the note
    liftIO $ putStrLn "Querying for my notes..."
    let f = defaultFilter 
          { filterAuthors = Just [pubKey]
          , filterKinds = Just [1]
          , filterLimit = Just 5
          }
          
    events <- queryEvents f
    
    liftIO $ do
      putStrLn $ "Found " ++ show (length events) ++ " events."
      forM_ events $ \e -> do
        putStrLn $ " - " ++ show (eventContent e)

  -- 5. Cleanup
  putStrLn "Disconnecting..."
  disconnect env
  putStrLn "Done."
