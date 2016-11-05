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

type Multipath = [String]

separatorChar :: Char
separatorChar = '/'

escapeChar :: Char
escapeChar = '\\'

fromBytes :: [Word8] -> Either String Multipath
fromBytes = fromString . UTF8.decode

fromString :: String -> Either String Multipath
fromString = either (Left . show) (Right) . parse parseMultihash "mutipath"

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
