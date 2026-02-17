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
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO (hPutStrLn, stderr)
import System.Timeout (timeout)

import Nostr.Event
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

-- | Create an event builder for a short text note (kind 1)
shortNote :: Text -> EventBuilder
shortNote content = EventBuilder
  { ebKind    = 1
  , ebTags    = []
  , ebContent = content
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
-- ["p", <pubkey>]
withMention :: Text -> EventBuilder -> EventBuilder
withMention pubkey = withTag ["p", pubkey]

-- | Mention a user with a recommended relay url
-- ["p", <pubkey>, <relay_url>]
withMentionRelay :: Text -> Text -> EventBuilder -> EventBuilder
withMentionRelay pubkey relay = withTag ["p", pubkey, relay]

-- | Reference an addressable event ("a" tag)
-- ["a", <kind>:<pubkey>:<d-tag>]
withAddressRef :: Kind -> Text -> Text -> EventBuilder -> EventBuilder
withAddressRef kind pubkey dTag = 
  let addr = T.pack (show kind) <> ":" <> pubkey <> ":" <> dTag
  in withTag ["a", addr]

-- | Reference an addressable event with a recommended relay url
-- ["a", <kind>:<pubkey>:<d-tag>, <relay_url>]
withAddressRefRelay :: Kind -> Text -> Text -> Text -> EventBuilder -> EventBuilder
withAddressRefRelay kind pubkey dTag relay = 
  let addr = T.pack (show kind) <> ":" <> pubkey <> ":" <> dTag
  in withTag ["a", addr, relay]

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
