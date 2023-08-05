module Parsers(parseRequestLine) where

import Relude hiding (many)
import qualified ASCII as A
import Data.ByteString (StrictByteString)
import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec, crlf)
import ServerTypes
import qualified ASCII.Decimal as AD

parseRequestLine :: StrictByteString -> Either ParseError RequestLine 
parseRequestLine = parse requestLineParser ""

requestLineParser :: Parsec StrictByteString () RequestLine 
requestLineParser = do
    method <- methodParser
    void $ space
    target <- requestTargetParser
    void $ space
    httpV1_1 <- versionParser
    void $ crlf
    return $ RequestLine method target httpV1_1 
    
methodParser :: Parsec StrictByteString () Method 
methodParser = do
        meth <- many letter 
        case meth of 
            "GET" -> return $ Get
            _ -> fail "invalid method"

requestTargetParser :: Parsec StrictByteString () RequestTarget
requestTargetParser = do
     requestTarget <- many1 $ choice [alphaNum, char '/']
     case A.validateString $ fromString requestTarget of
         Just asciiStr -> return asciiStr
         _ -> fail "invalid request target"  

versionParser :: Parsec StrictByteString () Version
versionParser = do
    version <- string "HTTP/1.1"
    if version == "HTTP/1.1" then return $ Version AD.Digit1 AD.Digit1 else fail "invalid version"
