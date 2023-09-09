{- cabal:

build-depends: megaparsec, bytestring, text, base
default-language: GHC2021
    
-}

{-# LANGUAGE OverloadedStrings #-}

module Parsers.CabalFile where

import Data.ByteString qualified      as BS
import           Data.Char            (chr)
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Text.Megaparsec      (MonadParsec (eof, notFollowedBy, takeP, try),
                                       Parsec, choice, many, parse, someTill,
                                       (<|>), takeWhileP, manyTill)
import Text.Megaparsec.Byte qualified as B
import Data.String (fromString)

data CabalFile = MkCF CFTopLevel [CFStanza] deriving (Eq, Show)

newtype CFTopLevel = MkCFTopLevel [CFTopLevelData] deriving (Eq, Show)

data CFStanza deriving (Eq, Show)

data CFTopLevelData
    = CFTLRaw BS.ByteString
    | CFTLCabalVersion VersionNumber
    | CFTLComment Comment
    | CFTLName BS.ByteString
    deriving (Eq, Show)

type VersionNumber = [Int]

newtype Comment = MkComment Text deriving (Eq, Show)

type Parser = Parsec Void BS.ByteString

breakOnStanza :: Parser ()
breakOnStanza = do
    B.eol
    choice
        [ B.string "executable"
        , B.string "common"
        , B.string "library"
        , B.string "test-suite"
        , B.string "benchmark"
        ]
    pure ()

parseCabalFileForTopLevel :: Parser BS.ByteString
parseCabalFileForTopLevel = do
    mainReturn <- many (notFollowedBy (try breakOnStanza <|> eof) >> takeP Nothing 1)
    next <- try (takeP Nothing 1) <|> pure BS.empty
    pure $ BS.concat mainReturn <> next

parseCabalFile :: BS.ByteString -> CabalFile
parseCabalFile file = MkCF (MkCFTopLevel topLevelFields) []
    where
        (Right topLevel) = parse parseCabalFileForTopLevel "" file
        (Right topLevelFields) = parse cabalTopLevel "" topLevel

cabalTopLevel :: Parser [CFTopLevelData]
cabalTopLevel = do
    many (choice
        [ try parseCabalVersion
        , try parsePackageName
        , try parseComment
        , try parseRaw
        ] )

parseCabalVersion :: Parser CFTopLevelData
parseCabalVersion = do
    B.space
    B.string "cabal-version:"
    B.space
    list <- many (someTill B.digitChar (try (B.char 46) <|> B.spaceChar))
    B.space
    pure . CFTLCabalVersion $ read . fmap (chr . fromIntegral) <$> list

parseRaw :: Parser CFTopLevelData
parseRaw = do
    content <- manyTill (takeP Nothing 1) B.eol
    pure . CFTLRaw $ BS.concat content

parsePackageName :: Parser CFTopLevelData
parsePackageName = do
    B.space
    B.string "name:"
    B.space
    name <- manyTill (takeP Nothing 1) (try B.spaceChar)
    B.space
    pure . CFTLName $ BS.concat name

parseComment :: Parser CFTopLevelData
parseComment = do
    B.space
    B.string "--"
    B.space
    comment <- manyTill (takeP Nothing 1) (try B.eol)
    pure . CFTLComment . MkComment . fromString . show $ BS.concat comment