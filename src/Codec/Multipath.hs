module Codec.Multipath (
    Multipath (Multipath), fromString,
    toByteString, fromByteString
) where

import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Parsec.Error
import Data.Either
import Control.Monad.Except
import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8

data Multipath = Multipath [String] deriving (Eq, Ord)

instance Show Multipath where
    show = toString

toByteString :: Multipath -> ByteString
toByteString = UTF8.fromString . toString

fromByteString :: MonadError String m => ByteString -> m Multipath
fromByteString b = fromString $ UTF8.toString b

separatorChar :: Char
separatorChar = '/'

escapeChar :: Char
escapeChar = '\\'

fromString :: (MonadError String m) => String -> m Multipath
fromString s = either (throwError . show) (return) $ (parse parseMultipath s) s

toString :: Multipath -> String
toString (Multipath a) = concat . map ((separatorChar :) . concat . map escape) $ a where
    escape ch | ch == separatorChar = [escapeChar, separatorChar]
              | ch == escapeChar = [escapeChar, escapeChar]
              | otherwise = [ch]

parseMultipath :: Parser Multipath
parseMultipath = do
    parts <- many $ do
        char separatorChar
        parsePart
    eof
    return $ Multipath parts

parsePart :: Parser String
parsePart = many $ (noneOf [separatorChar, escapeChar] <|> parseEscapedChar)

parseEscapedChar :: Parser Char
parseEscapedChar = do
    char escapeChar
    anyChar
