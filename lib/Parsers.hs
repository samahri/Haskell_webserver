module Parsers(parseRequestLine) where

import Relude hiding (many)
import qualified ASCII as A
import Data.ByteString (StrictByteString)
import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec, crlf)
import ServerTypes
import qualified ASCII.Decimal as AD

data ServerError = ServerRequestError ErrorType Text deriving Show
data ErrorType = InvalidHTTPMethodError | ParseError deriving Show

parseRequestLine :: StrictByteString -> Either ServerError RequestLine 
parseRequestLine line = first toServerError result
    where
      result = parse requestLineParser "" line
      toServerError err = ServerRequestError ParseError (show err)

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
            "GET" -> return Get
            _ -> fail "invalid method" 

requestTargetParser :: Parsec StrictByteString () RequestTarget
requestTargetParser = do
     requestTarget <- many1 $ noneOf [' '] 
     case A.validateString $ fromString requestTarget of
         Just asciiStr -> return asciiStr
         _ -> fail "invalid request target"  

versionParser :: Parsec StrictByteString () Version
versionParser = do
    void $ label (string "HTTP/1.1") (fail "invalid version")
    return $ Version AD.Digit1 AD.Digit1 
