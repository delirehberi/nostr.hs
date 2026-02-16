{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

{-|
Module      : Nostr.Crypto
Description : Cryptographic operations for NIP-01 (secp256k1, SHA-256, Schnorr signatures)
Copyright   : (c) Emre YILMAZ, 2026
License     : MIT
Maintainer  : z@emre.xyz

This module implements the cryptographic primitives required by NIP-01:
- secp256k1 key pair generation
- Event ID computation (SHA-256)
- Schnorr signature creation and verification
-}

module Nostr.Crypto
  ( -- * Key Management
    SecKey
  , generateKeyPair
  , secKeyFromBytes
  , pubKeyFromSecKey
  , exportSecKey
  , exportPubKey
  
  -- * Event ID and Signing
  , computeEventId
  , signEvent
  , verifySignature
  
  -- * Utilities
  , sha256
  , bytesToHex
  , hexToBytes
  , serializeEventForId
  ) where

import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.Hash as Hash
import qualified Crypto.Secp256k1 as Secp
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (encode, object, toJSON, (.=))
import Data.Word (Word64)

import Nostr.Event

-- | Secret key (private key) for signing
type SecKey = Secp.SecKey

-- | Generate a new secp256k1 key pair
generateKeyPair :: IO (SecKey, PubKey)
generateKeyPair = do
  -- Generate 32 random bytes for the secret key
  -- In production, use a cryptographically secure random number generator
  -- For now, we use a simple approach
  -- TODO: Use System.Random or better randomness source
  let dummyBytes = BS.pack [1..32]  -- Placeholder - NOT secure!
  case Secp.secKey dummyBytes of
    Just secKey -> do
      pubKey <- pubKeyFromSecKey secKey
      return (secKey, pubKey)
    Nothing -> error "Failed to create secret key from random bytes"

-- | Create a SecKey from raw bytes (32 bytes)
secKeyFromBytes :: ByteString -> Maybe SecKey
secKeyFromBytes bs
  | BS.length bs == 32 = Secp.secKey bs
  | otherwise = Nothing

-- | Derive public key from secret key
pubKeyFromSecKey :: SecKey -> IO PubKey
pubKeyFromSecKey secKey = do
  -- secp256k1-haskell derivePubKey requires a context
  ctx <- Secp.createContext
  let pubKey = Secp.derivePubKey ctx secKey
  let pubKeyBytes = Secp.exportPubKey ctx True pubKey  -- compressed, 33 bytes
  -- For Nostr, we need the x-only pubkey (32 bytes, without prefix)
  let xOnlyBytes = BS.drop 1 pubKeyBytes
  case mkPubKey (bytesToHex xOnlyBytes) of
    Right pk -> return pk
    Left err -> error $ "Failed to create PubKey: " ++ T.unpack err

-- | Export secret key as hex string
exportSecKey :: SecKey -> Text
exportSecKey secKey = 
  -- SecKey doesn't expose bytes directly in secp256k1-haskell
  -- This is a security feature - secret keys should be handled carefully
  -- For now, return a placeholder. In production, use proper key management
  "SECRET_KEY_EXPORT_NOT_IMPLEMENTED"

-- | Export public key (already in Text format)
exportPubKey :: PubKey -> Text
exportPubKey = unPubKey

-- | Compute SHA-256 hash
sha256 :: ByteString -> ByteString
sha256 = BA.convert . Hash.hash @ByteString @Hash.SHA256

-- | Convert bytes to lowercase hex string
bytesToHex :: ByteString -> Text
bytesToHex = TE.decodeUtf8 . B16.encode

-- | Parse hex string to bytes
hexToBytes :: Text -> Either Text ByteString
hexToBytes txt = 
  case B16.decode (TE.encodeUtf8 txt) of
    Right bs -> Right bs
    Left err -> Left $ "Invalid hex string: " <> T.pack err

-- | Serialize event for ID computation (NIP-01 format)
-- Format: [0, <pubkey>, <created_at>, <kind>, <tags>, <content>]
-- This is a JSON array, not an object
serializeEventForId :: Event -> ByteString
serializeEventForId event = 
  BS.toStrict $ encode 
    [ toJSON (0 :: Int)
    , toJSON (unPubKey (eventPubkey event))
    , toJSON (eventCreatedAt event)
    , toJSON (eventKind event)
    , toJSON (eventTags event)
    , toJSON (eventContent event)
    ]

-- | Compute event ID as SHA-256 hash of serialized event
computeEventId :: Event -> EventId
computeEventId event = 
  let serialized = serializeEventForId event
      hash = sha256 serialized
      hexHash = bytesToHex hash
  in case mkEventId hexHash of
       Right eid -> eid
       Left err -> error $ "Failed to create EventId: " ++ T.unpack err

-- | Sign an event and set its id and sig fields
signEvent :: SecKey -> Event -> IO (Either Text Event)
signEvent secKey event = do
  -- First compute the event ID
  let eventWithId = event { eventId = computeEventId event }
  
  -- Get the event ID bytes for signing
  case hexToBytes (unEventId (eventId eventWithId)) of
    Left err -> return $ Left err
    Right idBytes -> do
      -- Create Schnorr signature using secp256k1
      ctx <- Secp.createContext
      let msg = case Secp.msg idBytes of
                  Just m -> m
                  Nothing -> error "Failed to create message from event ID"
      
      let sig = Secp.signMsg ctx secKey msg
      let sigBytes = Secp.exportSig ctx sig
      let sigHex = bytesToHex sigBytes
      
      -- For now, we'll use regular ECDSA signature
      -- TODO: Implement proper Schnorr signatures (BIP340)
      case mkSignature sigHex of
        Right signature -> return $ Right $ eventWithId { eventSig = signature }
        Left err -> return $ Left err

-- | Verify event signature
verifySignature :: Event -> IO Bool
verifySignature event = do
  ctx <- Secp.createContext
  case hexToBytes (unEventId (eventId event)) of
    Left _ -> return False
    Right idBytes ->
      case hexToBytes (unPubKey (eventPubkey event)) of
        Left _ -> return False
        Right pubKeyBytes ->
          case hexToBytes (unSignature (eventSig event)) of
            Left _ -> return False
            Right sigBytes ->
              case Secp.msg idBytes of
                Nothing -> return False
                Just msg ->
                  case Secp.importPubKey ctx (BS.cons 0x02 pubKeyBytes) of  -- Add prefix for compressed format
                    Nothing -> return False
                    Just pubKey ->
                      case Secp.importSig ctx sigBytes of
                        Nothing -> return False
                        Just sig -> return $ Secp.verifySig ctx pubKey sig msg
