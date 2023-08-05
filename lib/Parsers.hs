module Parsers(parseRequestLine) where

import Relude hiding (many)
import Data.ByteString (StrictByteString)
import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec)
import ServerTypes

parseRequestLine :: StrictByteString -> Either ParseError Method 
parseRequestLine = parse requestLineParser ""

requestLineParser :: Parsec StrictByteString () Method
requestLineParser = do
    method <- methodParser
    return method 
    
methodParser :: Parsec StrictByteString () Method 
methodParser = do
        meth <- many letter 
        void $ space
        case meth of 
            "GET" -> return $ Get
            _ -> fail "invalid method"

