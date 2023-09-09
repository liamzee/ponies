{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_ponies (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "ponies"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Prototype of GUI wrapper around Cabal"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
