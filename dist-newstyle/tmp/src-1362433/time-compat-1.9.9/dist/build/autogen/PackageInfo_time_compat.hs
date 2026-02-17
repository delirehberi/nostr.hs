{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_time_compat (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "time_compat"
version :: Version
version = Version [1,9,9] []

synopsis :: String
synopsis = "Compatibility package for time"
copyright :: String
copyright = ""
homepage :: String
homepage = "https://github.com/haskellari/time-compat"
