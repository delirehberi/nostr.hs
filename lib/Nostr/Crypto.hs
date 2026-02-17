{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

{-|
Module      : Nostr.Crypto
Description : Cryptographic operations for Nostr events using BIP-340 Schnorr signatures
Copyright   : (c) Emre YILMAZ, 2026
License     : MIT
Maintainer  : z@emre.xyz

This module provides cryptographic functions for Nostr events,
including key generation, event ID computation, and BIP-340 Schnorr signatures.
-}

module Nostr.Crypto
  ( -- * Key Management
    SecKey
  , generateKeyPair
  , pubKeyFromSecKey
  , exportSecKey
  , exportPubKey
  , secKeyFromBytes
  
  -- * Event Operations
  , computeEventId
  , signEvent
  , verifySignature
  , serializeEventForId
  
  -- * Utilities
  , bytesToHex
  , hexToBytes
  ) where

import qualified Crypto.Hash.Algorithms as Hash
import qualified Crypto.Hash as Hash
import qualified Crypto.Curve.Secp256k1 as Secp
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Aeson (encode, object, toJSON, (.=))
-- import Data.Word (Word64) -- Hiding this to import from GHC.Word
import qualified System.Entropy as E
import Data.Word.Wider (Wider(..))
import Data.Word.Limb (Limb(..))
import GHC.Word (Word64(..), Word(..))

import Nostr.Event

-- | Secret key (private key) for signing - 32-byte wide word
type SecKey = Wider

-- | Generate a new secp256k1 key pair for Schnorr signatures
generateKeyPair :: IO (SecKey, PubKey)
generateKeyPair = do
  -- Generate 32 random bytes for the secret key
  secKeyBytes <- E.getEntropy 32
  case secKeyFromBytes secKeyBytes of
    Just secKey -> do
      pubKey <- pubKeyFromSecKey secKey
      return (secKey, pubKey)
    Nothing -> error "Failed to create secret key from random bytes"

-- | Derive public key from secret key (x-only 32-byte for BIP-340)
pubKeyFromSecKey :: SecKey -> IO PubKey
pubKeyFromSecKey secKey = do
  -- ppad-secp256k1 uses derive_pub to create public key from secret key
  -- It returns Maybe Pub (Projective point)
  case Secp.derive_pub secKey of
    Just pubKey -> do
      -- Serialize to compressed format (33 bytes) and drop the first byte
      -- to get the 32-byte x-only public key as per BIP-340
      let compressed = Secp.serialize_point pubKey
      let pubKeyBytes = BS.drop 1 compressed
      case mkPubKey (bytesToHex pubKeyBytes) of
        Right pk -> return pk
        Left err -> error $ "Failed to create PubKey: " ++ T.unpack err
    Nothing -> error "Failed to derive public key"

-- | Create a SecKey from raw bytes (32 bytes)
secKeyFromBytes :: ByteString -> Maybe SecKey
secKeyFromBytes bs
  | BS.length bs == 32 = 
      -- Convert 32 bytes to Wider Word64
      let (w0, r1) = parseWord64 (BS.take 8 bs)
          (w1, r2) = parseWord64 (BS.take 8 r1)
          (w2, r3) = parseWord64 (BS.take 8 r2)
          (w3, _)  = parseWord64 (BS.take 8 r3)
          
          unW (W# w) = w
          toLimb :: Word64 -> Limb
          toLimb w = Limb (unW (fromIntegral w :: Word))
          
          l0 = toLimb w0
          l1 = toLimb w1
          l2 = toLimb w2
          l3 = toLimb w3
      in Just $ Wider (# l0, l1, l2, l3 #)
  | otherwise = Nothing
  where
    parseWord64 :: ByteString -> (Word64, ByteString)
    parseWord64 b = 
      let ws = BS.unpack b
          w = foldr (\byte acc -> acc * 256 + fromIntegral byte) 0 ws
      in (w, BS.drop 8 b)

-- | Export secret key as hex string
-- Note: In production, secret keys should never be exported
exportSecKey :: SecKey -> Text
exportSecKey _ = "SECRET_KEY_EXPORT_NOT_IMPLEMENTED"

-- | Export public key (already in Text format)
exportPubKey :: PubKey -> Text
exportPubKey = unPubKey

-- ============================================================================
-- Event ID Computation
-- ============================================================================

-- | Compute SHA-256 hash
sha256 :: ByteString -> ByteString
sha256 = BA.convert . Hash.hash @ByteString @Hash.SHA256

-- | Serialize event for ID computation
-- Returns JSON array: [0, pubkey, created_at, kind, tags, content]
serializeEventForId :: Event -> ByteString
serializeEventForId event =
  let arr =
        [ toJSON (0 :: Int)
        , toJSON (unPubKey (eventPubkey event))
        , toJSON (eventCreatedAt event)
        , toJSON (eventKind event)
        , toJSON (eventTags event)
        , toJSON (eventContent event)
        ]
  in BS.toStrict $ encode arr

-- | Compute event ID using SHA-256
computeEventId :: Event -> EventId
computeEventId event = 
  let serialized = serializeEventForId event
      hash = sha256 serialized
      hexHash = bytesToHex hash
  in case mkEventId hexHash of
       Right eid -> eid
       Left err -> error $ "Failed to create EventId: " ++ T.unpack err

-- ============================================================================
-- BIP-340 Schnorr Signatures
-- ============================================================================

-- | Sign an event and set its id and sig fields using BIP-340 Schnorr signatures
signEvent :: SecKey -> Event -> IO (Either Text Event)
signEvent secKey event = do
  -- First compute the event ID
  let eventWithId = event { eventId = computeEventId event }
  
  -- Get the event ID bytes for signing (must be 32 bytes for Schnorr)
  case hexToBytes (unEventId (eventId eventWithId)) of
    Left err -> return $ Left err
    Right idBytes -> do
      -- Generate 32 bytes of auxiliary randomness (recommended by BIP-340)
      auxRand <- E.getEntropy 32
      
      -- Sign with BIP-340 Schnorr
      case Secp.sign_schnorr secKey idBytes auxRand of
        Nothing -> return $ Left "Failed to create Schnorr signature"
        Just sigBytes -> do
          let sigHex = bytesToHex sigBytes
          
          -- Create Signature with proper validation (should be 64 bytes = 128 hex chars)
          case mkSignature sigHex of
            Right signature -> return $ Right $ eventWithId { eventSig = signature }
            Left err -> return $ Left err

-- | Verify event signature using BIP-340 Schnorr verification
verifySignature :: Event -> IO Bool
verifySignature event = do
  case hexToBytes (unEventId (eventId event)) of
    Left _ -> return False
    Right idBytes ->
      case hexToBytes (unPubKey (eventPubkey event)) of
        Left _ -> return False
        Right pubKeyBytes ->
          case hexToBytes (unSignature (eventSig event)) of
            Left _ -> return False
            Right sigBytes -> do
              -- Parse public key from x-only bytes
              -- BIP-340 uses implicit even Y coordinate (prefix 0x02)
              let compressedPubKey = BS.cons 0x02 pubKeyBytes
              case Secp.parse_point compressedPubKey of
                Nothing -> return False
                Just pubKeyPoint ->
                  -- Verify Schnorr signature
                  return $ Secp.verify_schnorr idBytes pubKeyPoint sigBytes

-- ============================================================================
-- Utility Functions
-- ============================================================================

-- | Convert bytes to lowercase hex string
bytesToHex :: ByteString -> Text
bytesToHex = TE.decodeUtf8 . B16.encode

-- | Convert hex string to bytes
hexToBytes :: Text -> Either Text ByteString
hexToBytes hexText =
  case B16.decode (TE.encodeUtf8 hexText) of
    Right bs -> Right bs
    Left err -> Left $ "Invalid hex string: " <> T.pack err
