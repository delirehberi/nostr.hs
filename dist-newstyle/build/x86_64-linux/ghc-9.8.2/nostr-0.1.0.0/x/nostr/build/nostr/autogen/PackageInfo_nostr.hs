{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_nostr (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "nostr"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Nostr library"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://nostrhs.emre.xyz"
