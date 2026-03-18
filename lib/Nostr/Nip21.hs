{-# LANGUAGE OverloadedStrings #-}

module Nostr.Nip21
  ( -- * URI Handling
    parseNostrUri
  , toNostrUri
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Nostr.Nip19 (Nip19Object(..), ProfilePtr(..), EventPtr(..), decode, toNpub, toNsec, toNote, toNprofile, toNevent)

-- | Parse a nostr: URI into a Nip19Object
-- Strips the "nostr:" prefix and decodes the NIP-19 string
parseNostrUri :: Text -> Either Text Nip19Object
parseNostrUri uri
  | "nostr:" `T.isPrefixOf` uri = decode (T.drop 6 uri)
  | otherwise = Left "URI does not start with 'nostr:'"

-- | Generate a nostr: URI from a Nip19Object
-- Adds the "nostr:" prefix to the NIP-19 encoded string
toNostrUri :: Nip19Object -> Maybe Text
toNostrUri obj = do
  encoded <- case obj of
    Nip19Pub pk -> toNpub pk
    Nip19Sec sk -> toNsec sk
    Nip19Note eid -> toNote eid
    Nip19Profile (ProfilePtr pk relays) -> toNprofile pk relays
    Nip19Event (EventPtr eid relays author) -> toNevent eid relays author
  return ("nostr:" <> encoded)