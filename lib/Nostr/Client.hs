{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nostr.Client 
  ( -- * Application
    NostrEnv(..)
  , NostrApp
  , runNostrApp
  , connectRelays
  , disconnect
    -- * Event Builder
  , EventBuilder(..)
  , shortNote
  , withKind
  , withTag
  -- * Standard Tag Combinators (NIP-01)
  , withReply
  , withReplyTo
  , withMention
  , withMentionRelay
  , withAddressRef
  , withAddressRefRelay
    -- * Contact Lists (NIP-02)
  , Contact(..)
  , getContacts
  , follow
  , unfollow
    -- * Helpers
  , parsePubKey
    -- * Publishing
  , publish
  , publishEvent
  , publishShortNote
    -- * Querying
  , queryEvents
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (catch, SomeException)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Data.Function ((&))
import Data.List (nub)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import Nostr.Event
import qualified Nostr.Event (mkPubKey)
import Nostr.Nip19 (decode, Nip19Object(..))
import Nostr.Relay
import Nostr.Crypto (Keys(..), signEvent)

-- | Nostr Application Environment
data NostrEnv = NostrEnv
  { envRelays :: [RelayConnection]
  }

-- | Nostr Application Monad
type NostrApp = ReaderT NostrEnv IO

-- | Run the Nostr Application
runNostrApp :: NostrEnv -> NostrApp a -> IO a
runNostrApp = flip runReaderT

-- | Connect to a list of relays
connectRelays :: [Text] -> IO NostrEnv
connectRelays urls = do
  conns <- mapMaybeM connectSafe urls
  return $ NostrEnv conns
  where
    connectSafe :: Text -> IO (Maybe RelayConnection)
    connectSafe url = do
      -- connectRelay returns immediately (forks background thread)
      conn <- connectRelay url
      hPutStrLn stderr $ "Initiated connection to " ++ T.unpack url
      return $ Just conn

    mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
    mapMaybeM f xs = catMaybes <$> mapM f xs

-- | Disconnect from all relays
disconnect :: NostrEnv -> IO ()
disconnect env = do
  forM_ (envRelays env) $ \conn -> do
    closeConnection conn

-- | Publish an event to all connected relays
publishEvent :: Event -> NostrApp ()
publishEvent event = do
  relays <- asks envRelays
  liftIO $ forM_ relays $ \conn -> do
    let msg = CMEvent event
    catch (sendMessage conn msg) (\(_ :: SomeException) -> 
      hPutStrLn stderr $ "Failed to send event to " ++ T.unpack (relayUrl conn))

-- | Query events from all relays and aggregate results
queryEvents :: Filter -> NostrApp [Event]
queryEvents filter = do
  relays <- asks envRelays
  -- We use mapConcurrently to query all relays in parallel
  results <- liftIO $ mapConcurrently (queryRelaySafe filter) relays
  -- Deduplicate events based on ID
  return $ nub $ concat results
  where
    queryRelaySafe :: Filter -> RelayConnection -> IO [Event]
    queryRelaySafe f conn = do
      catch (queryRelay conn f) (\e -> do
        hPutStrLn stderr $ "Error querying " ++ T.unpack (relayUrl conn) ++ ": " ++ show (e :: SomeException)
        return [])

-- | Helper to query a single relay
-- Sends REQ, collects events until EOSE or timeout (5 seconds).
queryRelay :: RelayConnection -> Filter -> IO [Event]
queryRelay conn filter = do
  subId <- SubscriptionId . T.pack . show . round <$> getPOSIXTime
  sendMessage conn (CMReq subId [filter])
  -- Set a 5-second timeout for the query
  result <- timeout 5000000 $ collectEvents conn subId []
  case result of
    Just events -> return events
    Nothing -> do
      hPutStrLn stderr $ "Query timed out for " ++ T.unpack (relayUrl conn)
      return []

collectEvents :: RelayConnection -> SubscriptionId -> [Event] -> IO [Event]
collectEvents conn subId acc = do
  msg <- receiveMessage conn
  case msg of
    RMEvent sid event -> 
      if sid == subId 
        then collectEvents conn subId (event : acc)
        else collectEvents conn subId acc -- Ignore other subscriptions
    RMEOSE sid -> 
      if sid == subId 
        then return acc 
        else collectEvents conn subId acc
    _ -> collectEvents conn subId acc -- Ignore other messages

-- ============================================================================
-- Event Builder
-- ============================================================================

-- | Builder for constructing Nostr events
-- Use 'shortNote' to create a builder, then chain with '&' and combinators.
--
-- @
-- import Data.Function ((&))
--
-- publish keys $ shortNote "Hello world"
--   & withKind 30023
--   & withTag ["e", "event-id"]
--   & withTag ["p", "pubkey-hex"]
-- @
data EventBuilder = EventBuilder
  { ebKind    :: Kind
  , ebTags    :: [Tag]
  , ebContent :: Text
  } deriving (Show, Eq)

mkEvent :: EventBuilder 
mkEvent = EventBuilder
  {
    ebKind = 1
  , ebTags = []
  , ebContent = ""
  }
-- | Create an event builder for a short text note (kind 1)
shortNote :: Text -> EventBuilder
shortNote content = mkEvent
  { 
    ebContent = content
  }

-- | Set the event kind
withKind :: Kind -> EventBuilder -> EventBuilder
withKind k eb = eb { ebKind = k }

-- | Append a tag to the event
withTag :: [Text] -> EventBuilder -> EventBuilder
withTag t eb = eb { ebTags = ebTags eb ++ [t] }

-- ============================================================================
-- NIP-01 Standard Tag Combinators
-- ============================================================================

-- | Reply to an event ("e" tag)
-- ["e", <event_id>]
withReply :: Text -> EventBuilder -> EventBuilder
withReply eid = withTag ["e", eid]

-- | Reply to an event with a recommended relay url
-- ["e", <event_id>, <relay_url>]
withReplyTo :: Text -> Text -> EventBuilder -> EventBuilder
withReplyTo eid relay = withTag ["e", eid, relay]

-- | Mention a user ("p" tag)
-- ["p", <pubkey_hex>]
withMention :: PubKey -> EventBuilder -> EventBuilder
withMention pubkey = withTag ["p", unPubKey pubkey]

-- | Mention a user with a recommended relay url
-- ["p", <pubkey_hex>, <relay_url>]
withMentionRelay :: PubKey -> Text -> EventBuilder -> EventBuilder
withMentionRelay pubkey relay = withTag ["p", unPubKey pubkey, relay]

-- | Reference an addressable event ("a" tag)
-- ["a", <kind>:<pubkey_hex>:<d-tag>]
withAddressRef :: Kind -> PubKey -> Text -> EventBuilder -> EventBuilder
withAddressRef kind pubkey dTag = 
  let addr = T.pack (show kind) <> ":" <> unPubKey pubkey <> ":" <> dTag
  in withTag ["a", addr]

-- | Reference an addressable event with a recommended relay url
-- ["a", <kind>:<pubkey_hex>:<d-tag>, <relay_url>]
withAddressRefRelay :: Kind -> PubKey -> Text -> Text -> EventBuilder -> EventBuilder
withAddressRefRelay kind pubkey dTag relay = 
  let addr = T.pack (show kind) <> ":" <> unPubKey pubkey <> ":" <> dTag
  in withTag ["a", addr, relay]

-- ============================================================================
-- Contact Lists (NIP-02)
-- ============================================================================

data Contact = Contact
  { contactPubkey  :: Text
  , contactRelay   :: Maybe Text
  , contactPetname :: Maybe Text
  } deriving (Show, Eq)

-- | Fetch the user's latest contact list (Kind 3)
getContacts :: Keys -> NostrApp [Contact]
getContacts keys = do
  let filter = defaultFilter
        { filterKinds   = Just [3]
        , filterAuthors = Just [keysPubKey keys]
        , filterLimit   = Just 1
        }
  events <- queryEvents filter
  
  -- We only care about the latest event if multiple returned (though limit 1 helps)
  -- But since queryEvents aggregates from multiple relays, we might get substitutes.
  -- We should sort by created_at.
  -- For now, just taking the first one if available.
  case events of
    [] -> return []
    (e:_) -> return $ mapMaybe parseContact (eventTags e)
  where
    parseContact :: Tag -> Maybe Contact
    parseContact tag = case tag of
      ("p" : pubkey : rest) -> 
        let relay   = if null rest then Nothing else Just (head rest)
            petname = if length rest > 1 then Just (rest !! 1) else Nothing
        in Just $ Contact pubkey relay petname
      _ -> Nothing

-- | Parse a public key from Hex or NIP-19 (npub) string
parsePubKey :: Text -> Maybe PubKey
parsePubKey t
  | "npub1" `T.isPrefixOf` t = case decode t of
      Right (Nip19Pub pk) -> Just pk
      _                   -> Nothing
  | otherwise = case Nostr.Event.mkPubKey t of
      Right pk -> Just pk
      _        -> Nothing

-- | Follow a user
follow :: Keys -> PubKey -> Maybe Text -> Maybe Text -> NostrApp ()
follow keys targetPubkey relay petname = do
  contacts <- getContacts keys
  
  -- Check if already following
  -- We compare the hex representation of pubkeys since Contact stores Text
  let targetHex = unPubKey targetPubkey
  let startContacts = Prelude.filter (\c -> contactPubkey c /= targetHex) contacts
  let newContact = Contact targetHex relay petname
  let newContacts = startContacts ++ [newContact]
  
  publishContacts keys newContacts

-- | Unfollow a user
unfollow :: Keys -> PubKey -> NostrApp ()
unfollow keys targetPubkey = do
  contacts <- getContacts keys
  let targetHex = unPubKey targetPubkey
  let newContacts = Prelude.filter (\c -> contactPubkey c /= targetHex) contacts
  publishContacts keys newContacts

-- | Helper to publish the contact list
publishContacts :: Keys -> [Contact] -> NostrApp ()
publishContacts keys contacts = do
  let builder = shortNote "" 
              & withKind 3
  
  -- Add all contacts as "p" tags
  let builderWithTags = foldl addContactTag builder contacts
  
  publish keys builderWithTags
  where
    addContactTag :: EventBuilder -> Contact -> EventBuilder
    addContactTag eb c = 
      let tags = ["p", contactPubkey c]
              ++ (case contactRelay c of Just r -> [r]; Nothing -> [""])
              ++ (case contactPetname c of Just p -> [p]; Nothing -> [])
      in withTag tags eb

-- | Build, sign, and publish an event from an EventBuilder
publish :: Keys -> EventBuilder -> NostrApp ()
publish keys eb = do
  now <- liftIO $ round <$> getPOSIXTime
  let unsigned = createUnsignedEvent (keysPubKey keys) now (ebKind eb) (ebTags eb) (ebContent eb)
  signedResult <- liftIO $ signEvent (keysSecKey keys) unsigned
  case signedResult of
    Right event -> publishEvent event
    Left err -> liftIO $ hPutStrLn stderr $ "Failed to sign event: " ++ T.unpack err

-- | Convenience: publish a kind-1 text note (no tags)
publishShortNote :: Keys -> Text -> NostrApp ()
publishShortNote keys content = publish keys (shortNote content)
