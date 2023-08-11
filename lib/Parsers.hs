module Parsers(parseRequestLine) where

import Relude as R hiding (many) 
import qualified ASCII as A
import Data.ByteString (StrictByteString)
import Text.ParserCombinators.Parsec
import Text.Parsec (Parsec, crlf)
import ServerTypes
import Data.Maybe (fromJust)

data ServerError = ServerRequestError ErrorType Text deriving Show
data ErrorType = InvalidHTTPMethodError | ParseError deriving Show

parseRequestLine :: StrictByteString -> Either ServerError Request 
parseRequestLine line = first toServerError result
    where
      result = parse httpRequestParser "" line
      toServerError err = ServerRequestError ParseError (show err)

httpRequestParser :: Parsec StrictByteString () Request 
httpRequestParser = do
    requestLine <- requestLineParser
    headers <- headersParser 
    body <- bodyParser 
    return $ Request requestLine headers body 

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
     case A.validateString $ R.fromString requestTarget of
         Just asciiStr -> return asciiStr
         _ -> fail "invalid request target"  

versionParser :: Parsec StrictByteString () Version
versionParser = do
    void $ label (string "HTTP/1.1") (fail "invalid version")
    return $ http1_1 

headersParser :: Parsec StrictByteString () [Field]
headersParser = do
    header <- manyTill singleHeaderParser crlf
    return header

singleHeaderParser :: Parsec StrictByteString () Field
singleHeaderParser = do
    fieldNameString <- manyTill (choice [alphaNum, char '-']) (char ':')
    fieldValueString <- manyTill anyChar crlf
    let fieldName = A.validateString $ R.fromString fieldNameString
    let fieldValue = A.validateString $ R.fromString fieldValueString
    if (isNothing fieldName || isNothing fieldValue)
      then fail "invalid field" 
      else return $ Field (fromJust fieldName) (fromJust fieldValue)
      
bodyParser :: Parsec StrictByteString () (Maybe Body)
bodyParser = do
    void $ many anyChar
    return Nothing
