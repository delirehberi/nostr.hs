{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_text_short (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "text_short"
version :: Version
version = Version [0,1,6,1] []

synopsis :: String
synopsis = "Memory-efficient representation of Unicode text strings"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
