{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Nostr.Relay
Description : NIP-01 client-relay communication types and utilities
Copyright   : (c) Emre YILMAZ, 2026
License     : MIT
Maintainer  : z@emre.xyz

This module defines the message types for client-relay communication
according to NIP-01 specification.
-}

module Nostr.Relay 
  ( -- * Core Types
    SubscriptionId(..)
  , ClientMessage(..)
  , RelayMessage(..)
  , Filter(..)
  , RelayConnection
  
  -- * Filter Helpers
  , defaultFilter
  
  -- * WebSocket Connection
  , connectRelay
  , sendMessage
  , receiveMessage
  , closeConnection
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, withArray, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Network.WebSockets as WS

import Nostr.Event

-- | Subscription ID used to identify subscriptions
-- Can be any arbitrary string chosen by the client
newtype SubscriptionId = SubscriptionId { unSubscriptionId :: Text }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON SubscriptionId where
  toJSON (SubscriptionId sid) = toJSON sid

instance FromJSON SubscriptionId where
  parseJSON v = SubscriptionId <$> parseJSON v

-- ============================================================================
-- Client Messages (Client → Relay)
-- ============================================================================

-- | Messages sent from client to relay
data ClientMessage
  = CMEvent Event                           -- ^ ["EVENT", <event>]
  | CMReq SubscriptionId [Filter]           -- ^ ["REQ", <subscription_id>, <filter1>, <filter2>, ...]
  | CMClose SubscriptionId                  -- ^ ["CLOSE", <subscription_id>]
  deriving (Show, Eq, Generic)

instance ToJSON ClientMessage where
  toJSON (CMEvent event) = 
    A.Array $ V.fromList [A.String "EVENT", A.toJSON event]
  
  toJSON (CMReq subId filters) = 
    A.Array $ V.fromList $ [A.String "REQ", A.toJSON subId] ++ map A.toJSON filters
  
  toJSON (CMClose subId) = 
    A.Array $ V.fromList [A.String "CLOSE", A.toJSON subId]

instance FromJSON ClientMessage where
  parseJSON = withArray "ClientMessage" $ \arr -> do
    when (V.length arr < 2) $
      fail "ClientMessage array must have at least 2 elements"
    
    msgType <- parseJSON (V.head arr) :: A.Parser Text
    
    case msgType of
      "EVENT" -> do
        when (V.length arr /= 2) $
          fail "EVENT message must have exactly 2 elements"
        event <- parseJSON (arr V.! 1)
        return $ CMEvent event
      
      "REQ" -> do
        when (V.length arr < 3) $
          fail "REQ message must have at least 3 elements (type, subscription_id, filter)"
        subId <- parseJSON (arr V.! 1)
        filters <- mapM parseJSON (V.toList $ V.drop 2 arr)
        return $ CMReq subId filters
      
      "CLOSE" -> do
        when (V.length arr /= 2) $
          fail "CLOSE message must have exactly 2 elements"
        subId <- parseJSON (arr V.! 1)
        return $ CMClose subId
      
      _ -> fail $ "Unknown client message type: " ++ T.unpack msgType
    where
      when :: Bool -> A.Parser () -> A.Parser ()
      when True action = action
      when False _ = return ()

-- ============================================================================
-- Relay Messages (Relay → Client)
-- ============================================================================

-- | Messages sent from relay to client
data RelayMessage
  = RMEvent SubscriptionId Event            -- ^ ["EVENT", <subscription_id>, <event>]
  | RMOK EventId Bool Text                  -- ^ ["OK", <event_id>, <accepted>, <message>]
  | RMEOSE SubscriptionId                   -- ^ ["EOSE", <subscription_id>]
  | RMClosed SubscriptionId Text            -- ^ ["CLOSED", <subscription_id>, <message>]
  | RMNotice Text                           -- ^ ["NOTICE", <message>]
  deriving (Show, Eq, Generic)

instance ToJSON RelayMessage where
  toJSON (RMEvent subId event) = 
    A.Array $ V.fromList [A.String "EVENT", A.toJSON subId, A.toJSON event]
  
  toJSON (RMOK eventId accepted msg) = 
    A.Array $ V.fromList [A.String "OK", A.toJSON eventId, A.toJSON accepted, A.toJSON msg]
  
  toJSON (RMEOSE subId) = 
    A.Array $ V.fromList [A.String "EOSE", A.toJSON subId]
  
  toJSON (RMClosed subId msg) = 
    A.Array $ V.fromList [A.String "CLOSED", A.toJSON subId, A.toJSON msg]
  
  toJSON (RMNotice msg) = 
    A.Array $ V.fromList [A.String "NOTICE", A.toJSON msg]

instance FromJSON RelayMessage where
  parseJSON = withArray "RelayMessage" $ \arr -> do
    when (V.length arr < 2) $
      fail "RelayMessage array must have at least 2 elements"
    
    msgType <- parseJSON (V.head arr) :: A.Parser Text
    
    case msgType of
      "EVENT" -> do
        when (V.length arr /= 3) $
          fail "EVENT message must have exactly 3 elements"
        subId <- parseJSON (arr V.! 1)
        event <- parseJSON (arr V.! 2)
        return $ RMEvent subId event
      
      "OK" -> do
        when (V.length arr /= 4) $
          fail "OK message must have exactly 4 elements"
        eventId <- parseJSON (arr V.! 1)
        accepted <- parseJSON (arr V.! 2)
        msg <- parseJSON (arr V.! 3)
        return $ RMOK eventId accepted msg
      
      "EOSE" -> do
        when (V.length arr /= 2) $
          fail "EOSE message must have exactly 2 elements"
        subId <- parseJSON (arr V.! 1)
        return $ RMEOSE subId
      
      "CLOSED" -> do
        when (V.length arr /= 3) $
          fail "CLOSED message must have exactly 3 elements"
        subId <- parseJSON (arr V.! 1)
        msg <- parseJSON (arr V.! 2)
        return $ RMClosed subId msg
      
      "NOTICE" -> do
        when (V.length arr /= 2) $
          fail "NOTICE message must have exactly 2 elements"
        msg <- parseJSON (arr V.! 1)
        return $ RMNotice msg
      
      _ -> fail $ "Unknown relay message type: " ++ T.unpack msgType
    where
      when :: Bool -> A.Parser () -> A.Parser ()
      when True action = action
      when False _ = return ()

-- ============================================================================
-- Filter
-- ============================================================================

-- | Filter for subscription queries
-- All fields are optional, matching events must satisfy all present conditions
data Filter = Filter
  { filterIds      :: Maybe [EventId]    -- ^ Event IDs to match
  , filterAuthors  :: Maybe [PubKey]     -- ^ Author public keys to match
  , filterKinds    :: Maybe [Kind]       -- ^ Event kinds to match
  , filterETags    :: Maybe [EventId]    -- ^ #e tags to match
  , filterPTags    :: Maybe [PubKey]     -- ^ #p tags to match
  , filterSince    :: Maybe Timestamp    -- ^ Events after this timestamp
  , filterUntil    :: Maybe Timestamp    -- ^ Events before this timestamp
  , filterLimit    :: Maybe Int          -- ^ Maximum number of events to return
  } deriving (Show, Eq, Generic)

-- | Default empty filter (matches everything)
defaultFilter :: Filter
defaultFilter = Filter
  { filterIds = Nothing
  , filterAuthors = Nothing
  , filterKinds = Nothing
  , filterETags = Nothing
  , filterPTags = Nothing
  , filterSince = Nothing
  , filterUntil = Nothing
  , filterLimit = Nothing
  }

instance ToJSON Filter where
  toJSON filter = object $ catMaybes
    [ ("ids" .=) <$> filterIds filter
    , ("authors" .=) <$> filterAuthors filter
    , ("kinds" .=) <$> filterKinds filter
    , ("#e" .=) <$> filterETags filter
    , ("#p" .=) <$> filterPTags filter
    , ("since" .=) <$> filterSince filter
    , ("until" .=) <$> filterUntil filter
    , ("limit" .=) <$> filterLimit filter
    ]
    where
      catMaybes :: [Maybe a] -> [a]
      catMaybes = foldr (\mx xs -> maybe xs (:xs) mx) []

instance FromJSON Filter where
  parseJSON = withObject "Filter" $ \v -> Filter
    <$> v .:? "ids"
    <*> v .:? "authors"
    <*> v .:? "kinds"
    <*> v .:? "#e"
    <*> v .:? "#p"
    <*> v .:? "since"
    <*> v .:? "until"
    <*> v .:? "limit"

-- ============================================================================
-- WebSocket Connection
-- ============================================================================

-- | Relay WebSocket connection
-- Contains the WebSocket connection for communicating with a relay
data RelayConnection = RelayConnection
  { relayUrl :: Text
  , relayConn :: WS.Connection
  }

-- | Connect to a Nostr relay via WebSocket
-- Takes a relay URL (e.g., "wss://relay.damus.io")
connectRelay :: Text -> IO RelayConnection
connectRelay url = do
  let (host, path) = parseRelayUrl url
  WS.runClient (T.unpack host) 443 (T.unpack path) $ \conn -> do
    return $ RelayConnection url conn
  where
    -- Parse relay URL to extract host and path
    -- Simple parser for wss://host/path format
    parseRelayUrl :: Text -> (Text, Text)
    parseRelayUrl u =
      let u' = case T.stripPrefix "wss://" u of
                 Just stripped -> stripped
                 Nothing -> case T.stripPrefix "ws://" u of
                              Just stripped -> stripped
                              Nothing -> u
          (host, pathWithSlash) = T.breakOn "/" u'
          path = if T.null pathWithSlash then "/" else pathWithSlash
      in (host, path)

-- | Send a client message to the relay
sendMessage :: RelayConnection -> ClientMessage -> IO ()
sendMessage conn msg = do
  let jsonMsg = A.encode msg
  WS.sendTextData (relayConn conn) jsonMsg

-- | Receive a relay message from the relay
-- This is a blocking operation that waits for the next message
receiveMessage :: RelayConnection -> IO (Either Text RelayMessage)
receiveMessage conn = do
  msgData <- WS.receiveData (relayConn conn) :: IO ByteString
  case A.eitherDecode msgData of
    Right relayMsg -> return $ Right relayMsg
    Left err -> return $ Left $ "Failed to parse relay message: " <> T.pack err

-- | Close the relay connection
closeConnection :: RelayConnection -> IO ()
closeConnection conn = 
  WS.sendClose (relayConn conn) ("Closing connection" :: Text)
