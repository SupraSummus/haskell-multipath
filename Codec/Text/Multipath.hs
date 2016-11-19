{-# LANGUAGE FlexibleContexts #-}

module Codec.Text.Multipath (
    Multipath,
    fromBytes, toBytes,
    fromString, toString
) where

import qualified Codec.Binary.UTF8.String as UTF8
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Error
import Data.Word
import Data.Either
import Control.Monad.Except

type Multipath = [String]

separatorChar :: Char
separatorChar = '/'

escapeChar :: Char
escapeChar = '\\'

fromBytes :: (MonadError String m) => [Word8] -> m Multipath
fromBytes = fromString . UTF8.decode

fromString :: (MonadError String m) => String -> m Multipath
fromString s = either (throwError . show) (return) $ (parse parseMultihash s) s

toBytes :: Multipath -> [Word8]
toBytes = UTF8.encode . toString

toString :: Multipath -> String
toString = concat . map ((separatorChar :) . concat . map escape) where
    escape ch | ch ==separatorChar = [escapeChar, separatorChar]
              | ch == escapeChar = [escapeChar, escapeChar]
              | otherwise = [ch]

parseMultihash :: Parser Multipath
parseMultihash = do
    parts <- many $ do
        char separatorChar
        parsePart
    eof
    return parts

parsePart :: Parser String
parsePart = many $ (noneOf [separatorChar, escapeChar] <|> parseEscapedChar)

parseEscapedChar :: Parser Char
parseEscapedChar = do
    char escapeChar
    anyChar
