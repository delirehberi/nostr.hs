{-# LANGUAGE OverloadedStrings #-}

module Nostr.Nip19
  ( -- * Encoding
    toNpub
  , toNsec
  , toNote
  , toNprofile
  , toNevent
    -- * Decoding
  , Nip19Object(..)
  , ProfilePtr(..)
  , EventPtr(..)
  , decode
  ) where

import Codec.Binary.Bech32 (dataPartFromBytes, dataPartToBytes, encode, decodeLenient, humanReadablePartFromText)
import qualified Codec.Binary.Bech32 as Bech32
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Word (Word8)

import Nostr.Crypto
import Nostr.Event (EventId(..), PubKey(..), mkPubKey, mkEventId)

-- | Decoded NIP-19 Object
data Nip19Object
  = Nip19Pub PubKey
  | Nip19Sec SecKey
  | Nip19Note EventId
  | Nip19Profile ProfilePtr
  | Nip19Event EventPtr
  deriving (Show)

instance Eq Nip19Object where
  (Nip19Pub a) == (Nip19Pub b) = a == b
  (Nip19Sec a) == (Nip19Sec b) = exportSecKey a == exportSecKey b
  (Nip19Note a) == (Nip19Note b) = a == b
  (Nip19Profile a) == (Nip19Profile b) = a == b
  (Nip19Event a) == (Nip19Event b) = a == b
  _ == _ = False

data ProfilePtr = ProfilePtr 
  { ppPubkey :: PubKey
  , ppRelays :: [Text]
  } deriving (Show, Eq)

data EventPtr = EventPtr
  { epEventId :: EventId
  , epRelays  :: [Text]
  , epAuthor  :: Maybe PubKey
  } deriving (Show, Eq)

-- | Encode a Public Key as npub
toNpub :: PubKey -> Maybe Text
toNpub pk = do
  pkBytes <- either (const Nothing) Just $ hexToBytes (exportPubKey pk)
  encodeBech32 "npub" pkBytes

-- | Encode a Secret Key as nsec
toNsec :: SecKey -> Maybe Text
toNsec sk = do
  skBytes <- either (const Nothing) Just $ hexToBytes (exportSecKey sk)
  encodeBech32 "nsec" skBytes

-- | Encode an Event ID as note
toNote :: EventId -> Maybe Text
toNote (EventId eid) = do
  eidBytes <- either (const Nothing) Just $ hexToBytes eid
  encodeBech32 "note" eidBytes

-- | Encode a profile pointer as nprofile (TLV)
toNprofile :: PubKey -> [Text] -> Maybe Text
toNprofile pk relays = do
  pkBytes <- either (const Nothing) Just $ hexToBytes (exportPubKey pk)
  -- TLV encoding: type (1 byte) + length (1 byte) + value
  -- Type 0: special (pubkey), 32 bytes
  -- Type 1: relay, variable length
  let tlv = encodeTLV 0 pkBytes <> mconcat (map (encodeTLV 1 . TE.encodeUtf8) relays)
  encodeBech32 "nprofile" tlv

-- | Encode an event pointer as nevent (TLV)
toNevent :: EventId -> [Text] -> Maybe PubKey -> Maybe Text
toNevent (EventId eid) relays author = do
  eidBytes <- either (const Nothing) Just $ hexToBytes eid
  
  authorBytes <- case author of
    Just pk -> do
       pkBytes <- either (const Nothing) Just $ hexToBytes (exportPubKey pk)
       return $ encodeTLV 2 pkBytes
    Nothing -> return BS.empty
    
  let tlv = encodeTLV 0 eidBytes 
         <> mconcat (map (encodeTLV 1 . TE.encodeUtf8) relays)
         <> authorBytes
  encodeBech32 "nevent" tlv

-- | Generic Bech32 Encoder
encodeBech32 :: Text -> ByteString -> Maybe Text
encodeBech32 prefix dataBytes = do
  hrp <- qualifiedHumanReadablePartFromText prefix
  let dataPart = dataPartFromBytes dataBytes
  either (const Nothing) Just $ encode hrp dataPart
  where
    qualifiedHumanReadablePartFromText t = 
      either (const Nothing) Just $ humanReadablePartFromText t

-- | TLV Encoder
-- Type (8-bit), Length (8-bit), Value
encodeTLV :: Word8 -> ByteString -> ByteString
encodeTLV typ val = 
  let len = fromIntegral (BS.length val) :: Word8
  in BS.pack [typ, len] <> val

-- | Decode a Bech32 string
decode :: Text -> Either Text Nip19Object
decode input = case decodeLenient input of
  Left err -> Left $ "Bech32 decode error: " <> T.pack (show err)
  Right (hrp, dataPart) -> do
    let prefix = Bech32.humanReadablePartToText hrp
    bytes <- maybe (Left "Invalid data part") Right $ dataPartToBytes dataPart
    
    case prefix of
      "npub" -> do
        pk <- parsePubKey bytes
        Right $ Nip19Pub pk
      "nsec" -> do
        sk <- parseSecKey bytes
        Right $ Nip19Sec sk
      "note" -> do
        eid <- parseEventId bytes
        Right $ Nip19Note eid
      "nprofile" -> do
        ptr <- parseProfileTLV bytes
        Right $ Nip19Profile ptr
      "nevent" -> do
        ptr <- parseEventTLV bytes
        Right $ Nip19Event ptr
      _ -> Left $ "Unknown prefix: " <> prefix

-- Helpers for parsing
parsePubKey :: ByteString -> Either Text PubKey
parsePubKey bs = 
  let hex = bytesToHex bs
  in Nostr.Event.mkPubKey hex

parseSecKey :: ByteString -> Either Text SecKey
parseSecKey bs = 
  case secKeyFromBytes bs of
    Just sk -> Right sk
    Nothing -> Left "Invalid secret key bytes"

parseEventId :: ByteString -> Either Text EventId
parseEventId bs = 
  let hex = bytesToHex bs
  in Nostr.Event.mkEventId hex

parseProfileTLV :: ByteString -> Either Text ProfilePtr
parseProfileTLV _ = Left "TLV parsing not implemented yet"

parseEventTLV :: ByteString -> Either Text EventPtr
parseEventTLV _ = Left "TLV parsing not implemented yet"
