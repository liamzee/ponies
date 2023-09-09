{-# LANGUAGE TemplateHaskell #-}

module Assets where

import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile, makeRelativeToProject)

{- | Standard web font these days, converted to bytestring courtesy File Embed. -}

proximaNova :: ByteString
proximaNova = $(makeRelativeToProject "app/Proxima Nova Font.otf" >>= embedFile)
