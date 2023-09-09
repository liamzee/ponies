{-# LANGUAGE StandaloneDeriving #-}

module Parsers.CabalFile2 where

import FlatParse.Stateful
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Kind (Type)

newtype CabalFile = MkCabalFile [Entry TopLevel]
    deriving (Eq, Show)

class Field a

data Entry fieldType where
    MkEntry
        :: Field fieldType
        => fieldType
        -> ByteString
        -> BeforeName
        -> AfterName
        -> AfterColon
        -> AfterData
        -> Entry fieldType
deriving instance Eq a => Eq (Entry a)
deriving instance Show a => Show (Entry a)

type BeforeName = Whitespace
type AfterName  = Whitespace
type AfterColon = Whitespace
type AfterData  = Whitespace 

newtype Whitespace = MkWhitespace ByteString
    deriving (Eq, Show)

data TopLevel
    = CabalVersion [Int]
    | PackageName ByteString
    | Maintainter ByteString
    | LibraryStanza [Entry LibraryFields]
    | ExecutableStanza ByteString [Entry ExecutableFields]
    | CommonStanza [Entry CommonStanzaFields]
    deriving (Eq, Show)

instance Field TopLevel

data LibraryFields
    deriving (Eq, Show)

instance Field LibraryFields

data ExecutableFields
    deriving (Eq, Show)

instance Field ExecutableFields

data CommonStanzaFields
    deriving (Eq, Show)

instance Field CommonStanzaFields