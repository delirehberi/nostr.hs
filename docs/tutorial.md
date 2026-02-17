# Nostr High-Level Client Tutorial

This document guides you through using the high-level Nostr client API in Haskell.

## 1. Setup

Ensure you have the library installed and dependencies set up in your `cabal` file.

```haskell
import Nostr.Client
import Nostr.Event
import Nostr.Relay
import Nostr.Crypto
import Nostr.Nip19 -- For Bech32 encoding/decoding
```

## 2. Connecting to Relays

Create a connection pool by providing a list of relay URLs.

```haskell
main :: IO ()
main = do
  env <- connectRelays ["wss://relay.damus.io", "wss://nos.lol"]
  runNostrApp env app
```

## 3. Managing Keys (NIP-19)

Generate keys or load them. You can now use NIP-19 Bech32 strings (`npub`, `nsec`).

```haskell
-- Generate new keys
(secKey, pubKey) <- generateKeyPair

-- Export to Bech32
case toNpub pubKey of
  Just npub -> putStrLn $ "My npub: " ++ T.unpack npub
  Nothing -> putStrLn "Error"

-- Import from Bech32 or Hex
let input = "npub1..." -- or hex
case parsePubKey input of
  Just pk -> do
    -- Use pk (PubKey type) in API functions
    follow keys pk Nothing Nothing
  Nothing -> putStrLn "Invalid key"
```

## 4. Publishing Events

Use `publish` with the `EventBuilder` pattern.

### Short Text Note (Kind 1)

```haskell
publishShortNote keys "Hello Nostr!"
```

### Complex Events with Tags

```haskell
publish keys $ shortNote "Replying to a note"
  & withReply "event-id-hex"
  & withMention pubKey -- Now takes PubKey type
```

## 5. Contact Lists (NIP-02)

Manage your follow list (Kind 3).

```haskell
-- Follow a user
case parsePubKey "npub1..." of
  Just pk -> follow keys pk (Just "wss://relay.damus.io") (Just "alice")
  Nothing -> return ()

-- Unfollow a user
unfollow keys pk

-- Get current contacts
contacts <- getContacts keys
mapM_ print contacts
```

## 6. Querying Events

Query relays for events.

```haskell
let filter = defaultFilter { filterKinds = Just [1], filterLimit = Just 10 }
events <- queryEvents filter
mapM_ (print . eventContent) events
```

## 7. Cleanup

Disconnect when done.

```haskell
disconnect env
```
