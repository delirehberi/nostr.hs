{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Nostr.Nip05 
  ( Nip05Id
  , verifyNip05
  , resolveNip05
  ) where

import Control.Exception (try, SomeException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, parseJSON, withObject, (.:?), (.:))
import qualified Data.Aeson as Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Simple 
  ( httpJSON
  , getResponseBody
  , parseRequest 
  , setRequestSecure
  , setRequestPort
  , Response
  )

import Nostr.Event (PubKey(..))
import Nostr.Client (parsePubKey)

type Nip05Id = Text

-- | JSON Response structure for NIP-05
-- { "names": { "bob": "pubkey_hex" } }
data Nip05Response = Nip05Response
  { nip05Names :: Map Text Text
  } deriving (Show)

instance FromJSON Nip05Response where
  parseJSON = withObject "Nip05Response" $ \v -> Nip05Response
    <$> v .: "names"

-- | Verify a NIP-05 identifier matches a given public key
verifyNip05 :: MonadIO m => Nip05Id -> PubKey -> m Bool
verifyNip05 identifier (PubKey pkHex) = do
  mPub <- resolveNip05 identifier
  case mPub of
    Just (PubKey resolvedHex) -> return $ pkHex == resolvedHex
    Nothing -> return False

-- | Resolve a NIP-05 identifier to a Public Key
resolveNip05 :: MonadIO m => Nip05Id -> m (Maybe PubKey)
resolveNip05 identifier = liftIO $ do
  let (name, domain) = parseIdentifier identifier
  let url = "https://" <> domain <> "/.well-known/nostr.json?name=" <> name
  
  -- PutStrLn for debugging
  -- putStrLn $ "Resolving NIP-05: " ++ T.unpack url
  
  req <- parseRequest (T.unpack url)
  -- Ensure HTTPS
  let req' = setRequestSecure True $ setRequestPort 443 req
  
  result <- try (httpJSON req') :: IO (Either SomeException (Response Nip05Response))
  
  case result of
    Left _ -> return Nothing
    Right response -> do
      let nip05Resp = getResponseBody response
      case Map.lookup name (nip05Names nip05Resp) of
        Just pubKeyHex -> return $ parsePubKey pubKeyHex
        Nothing        -> return Nothing

  where
    parseIdentifier :: Text -> (Text, Text)
    parseIdentifier t = 
      case T.splitOn "@" t of
        [n, d] -> (n, d)
        [d]    -> ("_", d) -- Handle domain-only as root? Spec says <name>@<domain> usually
        _      -> (t, "")  -- Invalid fallback
