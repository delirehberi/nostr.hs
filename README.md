# nostr.hs

A NIP-01 compliant Nostr library and client implementation in Haskell.

## Features

- **NIP-01 Compliant**: Full implementation of event structure, rules, and tags.
- **Schnorr Signatures**: Pure Haskell BIP-340 Schnorr signatures using `ppad-secp256k1`.
- **Relay Communication**: WebSocket-based communication with Nostr relays.
- **Type Safety**: Strong types for Event IDs, Public Keys, and Signatures.

## Implemented NIPs

- [x] NIP-01: Basic protocol flow
- [x] NIP-02: Contact Lists
- [x] NIP-05: DNS-based verification
- [x] NIP-09: Event Deletion Request
- [ ] NIP-10: Threading and Mentions
- [x] NIP-19: bech32-encoded entities
- [ ] NIP-21: nostr: URI scheme
- [ ] NIP-42: Relay Authentication


## Installation

This project uses **Nix** for reproducible builds.

1. **Clone the repository:**
   ```bash
   git clone https://github.com/delirehberi/nostr.hs.git
   cd nostr.hs
   ```

2. **Enter the development shell:**
   ```bash
   nix develop
   ```

3. **Build the project:**
   ```bash
   cabal build all
   ```

## Usage Examples

### High-Level API (Recommended)

The `Nostr.Client` module provides a monadic interface for managing connections, signing, and publishing. It handles robust reconnections (exponential backoff) and TLS usage automatically.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Nostr.Client
import Nostr.Event
import Nostr.Crypto
import Data.Function ((&))
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
  -- 1. Generate Keys
  -- Note: In a real app, you'd load these from safe storage
  (secKey, pubKey) <- generateKeyPair
  let keys = Keys secKey pubKey Nothing

  -- 2. Connect to Relays
  -- connectRelays establishes background threads for each relay and manages reconnection
  env <- connectRelays ["wss://relay.damus.io", "wss://nos.lol"]

  runNostrApp env $ do
    -- A. Publish a simple short text note (Kind 1)
    publishShortNote keys "Hello Nostr from Haskell!"

    -- B. Publish a complex event using the Builder Pattern
    -- Use the (&) operator to chain combinators for tags and kinds
    liftIO $ putStrLn "Publishing a reply..."
    publish keys $ shortNote "Replying to an event..."
      & withKind 1
      & withReply "event-id-hex"
      & withMention "pubkey-hex"
      & withTag ["t", "haskell"]
      
    -- C. Query Events
    -- Queries all connected relays concurrently and aggregates unique results
    let filter = defaultFilter
          { filterKinds = Just [1]
          , filterAuthors = Just [pubKey]
          , filterLimit = Just 10
          }
    
    events <- queryEvents filter
    liftIO $ print events

    -- D. Follow Users (NIP-02)
    -- This fetches your existing contact list (Kind 3), adds the user, and republishes.
    liftIO $ putStrLn "Following Jack..."
    follow keys "82341f882b6eabcd2ba7f1ef90aad961cf074af15b9ef44a09f9d2a8fbf71d22" (Just "wss://relay.damus.io") (Just "jack")

    -- E. Fetch Contacts
    contacts <- getContacts keys
    liftIO $ print contacts

    -- F. Delete Events (NIP-09)
    -- Deletes a list of event IDs with an optional reason.
    -- let eventIds = [EventId "hex_id_1...", EventId "hex_id_2..."]
    -- deleteEvents keys eventIds (Just "mistake")

  -- 3. Disconnect
  disconnect env
```

### Low-Level Event Creation

If you need manual control over event creation without the `NostrApp` environment:

```haskell
import Nostr.Event
import Nostr.Crypto
import Data.Time.Clock.POSIX (getPOSIXTime)

main :: IO ()
main = do
  (secKey, pubKey) <- generateKeyPair
  now <- round <$> getPOSIXTime
  
  -- Create unsigned event
  let unsigned = createUnsignedEvent pubKey now 1 [] "Manual event content"
  
  -- Sign event
  signed <- signEvent secKey unsigned
  case signed of
    Right event -> putStrLn $ "Event ID: " ++ show (eventId event)
    Left err    -> putStrLn $ "Error: " ++ show err
```

## Contributing

Contributions are welcome! Please ensure all tests pass before submitting a PR.
