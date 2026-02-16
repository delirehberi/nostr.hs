{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Nostr.Event
Description : NIP-01 compliant Event types and utilities
Copyright   : (c) Emre YILMAZ, 2026
License     : MIT
Maintainer  : z@emre.xyz

This module defines the core Event types according to NIP-01 specification.
-}

module Nostr.Event 
  ( -- * Core Types
    EventId(..)
  , PubKey(..)
  , Signature(..)
  , Timestamp
  , Kind
  , Tag
  , Event(..)
  
  -- * Constructors and Validators
  , mkEventId
  , mkPubKey
  , mkSignature
  , validateHex64
  , isValidHex
  
  -- * Event Creation
  , createUnsignedEvent
  , validateEvent
  , verifyEventId
  
  -- * Serialization
  , serializeForSigning
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, toJSON, withObject, (.:), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Data.Char (isHexDigit)
import GHC.Generics (Generic)

-- | Unix timestamp in seconds
type Timestamp = Word64

-- | Event kind as defined in NIP-01
-- Kinds determine how the event should be interpreted
type Kind = Int

-- | Tag is a JSON array of strings
-- Examples: ["e", "event_id"], ["p", "pubkey"]
type Tag = [Text]

-- | Event ID - 64-character lowercase hex string (32 bytes)
-- Computed as SHA-256 hash of the serialized event data
newtype EventId = EventId { unEventId :: Text }
  deriving (Show, Eq, Ord, Generic)

-- | Public key - 64-character lowercase hex string (32 bytes)
-- secp256k1 public key of the event creator
newtype PubKey = PubKey { unPubKey :: Text }
  deriving (Show, Eq, Ord, Generic)

-- | Schnorr signature - 128-character lowercase hex string (64 bytes)
-- Signature over the event ID
newtype Signature = Signature { unSignature :: Text }
  deriving (Show, Eq, Ord, Generic)

-- | Check if a character is a valid hex digit (0-9, a-f)
isValidHex :: Char -> Bool
isValidHex c = isHexDigit c && (c >= '0' && c <= '9' || c >= 'a' && c <= 'f')

-- | Validate that a Text is exactly 64 lowercase hex characters
validateHex64 :: Text -> Bool
validateHex64 t = T.length t == 64 && T.all isValidHex t

-- | Validate that a Text is exactly 128 lowercase hex characters (for signatures)
validateHex128 :: Text -> Bool
validateHex128 t = T.length t == 128 && T.all isValidHex t

-- | Smart constructor for EventId with validation
mkEventId :: Text -> Either Text EventId
mkEventId t
  | validateHex64 t = Right (EventId t)
  | otherwise = Left $ "Invalid EventId: must be 64 lowercase hex characters, got: " <> t

-- | Smart constructor for PubKey with validation
mkPubKey :: Text -> Either Text PubKey
mkPubKey t
  | validateHex64 t = Right (PubKey t)
  | otherwise = Left $ "Invalid PubKey: must be 64 lowercase hex characters, got: " <> t

-- | Smart constructor for Signature with validation
mkSignature :: Text -> Either Text Signature
mkSignature t
  | validateHex128 t = Right (Signature t)
  | otherwise = Left $ "Invalid Signature: must be 128 lowercase hex characters, got: " <> t

-- | Nostr Event as defined in NIP-01
-- All events must have these fields in this exact structure
data Event = Event
  { eventId        :: EventId      -- ^ 32-byte SHA-256 hash, lowercase hex
  , eventPubkey    :: PubKey       -- ^ 32-byte secp256k1 public key, lowercase hex
  , eventCreatedAt :: Timestamp    -- ^ Unix timestamp in seconds
  , eventKind      :: Kind         -- ^ Event kind (0-65535)
  , eventTags      :: [Tag]        -- ^ Array of tags (each tag is an array of strings)
  , eventContent   :: Text         -- ^ Arbitrary string content
  , eventSig       :: Signature    -- ^ 64-byte Schnorr signature, lowercase hex
  } deriving (Show, Eq, Generic)

-- | JSON serialization for EventId
instance ToJSON EventId where
  toJSON (EventId t) = toJSON t

instance FromJSON EventId where
  parseJSON v = do
    t <- parseJSON v
    case mkEventId t of
      Right eid -> return eid
      Left err -> fail (T.unpack err)

-- | JSON serialization for PubKey
instance ToJSON PubKey where
  toJSON (PubKey t) = toJSON t

instance FromJSON PubKey where
  parseJSON v = do
    t <- parseJSON v
    case mkPubKey t of
      Right pk -> return pk
      Left err -> fail (T.unpack err)

-- | JSON serialization for Signature
instance ToJSON Signature where
  toJSON (Signature t) = toJSON t

instance FromJSON Signature where
  parseJSON v = do
    t <- parseJSON v
    case mkSignature t of
      Right sig -> return sig
      Left err -> fail (T.unpack err)

-- | JSON serialization for Event (NIP-01 format)
instance ToJSON Event where
  toJSON event = object
    [ "id"         .= eventId event
    , "pubkey"     .= eventPubkey event
    , "created_at" .= eventCreatedAt event
    , "kind"       .= eventKind event
    , "tags"       .= eventTags event
    , "content"    .= eventContent event
    , "sig"        .= eventSig event
    ]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \v -> Event
    <$> v .: "id"
    <*> v .: "pubkey"
    <*> v .: "created_at"
    <*> v .: "kind"
    <*> v .: "tags"
    <*> v .: "content"
    <*> v .: "sig"

-- | Create an unsigned event (with dummy id and signature)
-- This should be signed using the Crypto module
createUnsignedEvent 
  :: PubKey      -- ^ Public key of the creator
  -> Timestamp   -- ^ Creation timestamp
  -> Kind        -- ^ Event kind
  -> [Tag]       -- ^ Tags
  -> Text        -- ^ Content
  -> Event
createUnsignedEvent pubkey created kind tags content = Event
  { eventId = EventId (T.replicate 64 "0")  -- Placeholder, to be computed
  , eventPubkey = pubkey
  , eventCreatedAt = created
  , eventKind = kind
  , eventTags = tags
  , eventContent = content
  , eventSig = Signature (T.replicate 128 "0")  -- Placeholder, to be computed
  }

-- | Validate that an event has well-formed fields
validateEvent :: Event -> Either Text ()
validateEvent event = do
  -- EventId validation (already validated by newtype)
  when (eventId event == EventId (T.replicate 64 "0")) $
    Left "Event has placeholder ID (not computed yet)"
  
  -- Signature validation
  when (eventSig event == Signature (T.replicate 128 "0")) $
    Left "Event has placeholder signature (not signed yet)"
  
  -- Kind validation (should be in valid range)
  when (eventKind event < 0 || eventKind event > 65535) $
    Left "Event kind must be between 0 and 65535"
  
  -- Timestamp validation (basic sanity check)
  when (eventCreatedAt event == 0) $
    Left "Event timestamp cannot be zero"
  
  return ()
  where
    when :: Bool -> Either Text () -> Either Text ()
    when True action = action
    when False _ = Right ()

-- | Verify that the event ID matches the computed hash
-- This will be implemented in Nostr.Crypto module
verifyEventId :: Event -> Bool
verifyEventId _event = 
  -- TODO: Implement in Crypto module
  -- computeEventId event == eventId event
  True

-- | Serialize event for signing (NIP-01 format)
-- Returns the JSON array: [0, <pubkey>, <created_at>, <kind>, <tags>, <content>]
-- This is used by the Crypto module to compute the event ID
serializeForSigning :: Event -> Value
serializeForSigning event = toJSON
  [ toJSON (0 :: Int)
  , toJSON (unPubKey (eventPubkey event))
  , toJSON (eventCreatedAt event)
  , toJSON (eventKind event)
  , toJSON (eventTags event)
  , toJSON (eventContent event)
  ]
