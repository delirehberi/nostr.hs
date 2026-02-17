{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_unordered_containers (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "unordered_containers"
version :: Version
version = Version [0,2,21] []

synopsis :: String
synopsis = "Efficient hashing-based container types"
copyright :: String
copyright = "2010-2014 Johan Tibell\n2010 Edward Z. Yang"
homepage :: String
homepage = "https://github.com/haskell-unordered-containers/unordered-containers"
