module Encoders(encodeOkResponse) where

import Relude 
import qualified ASCII as A
import ASCII.Char (Char(..))
import Data.ByteString (StrictByteString)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified ASCII.Decimal as AD
import ServerTypes

-- Response encoding
encodeResponse :: Response -> StrictByteString
encodeResponse (Response status fields bodyMaybe) = LBS.toStrict $ BSB.toLazyByteString $
    encodeStatusLine status 
    <> repeatedlyEncode (\x -> encodeField x <> encodeLineEnd) fields
    <> encodeLineEnd
    <> optionallyEncode encodeBody bodyMaybe

encodeOkResponse :: Body -> StrictByteString
encodeOkResponse = encodeResponse . okResponse

encodeStatusLine :: StatusLine -> BSB.Builder
encodeStatusLine (StatusLine version code reasonMaybe) =
    encodeVersion version <> A.fromCharList [Space] <> encodeStatusCode code 
    <> A.fromCharList [Space] <> optionallyEncode encodeReasonPhrase reasonMaybe <> encodeLineEnd

encodeStatusCode :: StatusCode -> BSB.Builder
encodeStatusCode (StatusCode x y z) = A.fromDigitList [x, y, z]

encodeReasonPhrase :: ReasonPhrase -> BSB.Builder
encodeReasonPhrase reason = BSB.byteString $ A.lift reason

okResponse :: Body -> Response
okResponse responseBody = Response okStatusLine fields (Just responseBody)
   where
     okStatusLine = StatusLine http_1_1 status200 (Just [A.string|OK|])
     fields = [contentTypeHtml, contentLength responseBody]

contentTypeHtml :: Field
contentTypeHtml = Field name value
  where
    name = [A.string|Content-Type|]
    value = [A.string|text/html; charset=utf-8|]

contentLength :: Body -> Field
contentLength body = Field name value
  where
    name = [A.string|Content-Length|]
    value = A.showIntegralDecimal $ LBS.length body

http_1_1 :: Version
http_1_1 = Version AD.Digit1 AD.Digit1

status200 :: StatusCode
status200 = StatusCode AD.Digit2 AD.Digit0 AD.Digit0

-- Request encodings
encodeRequestLine :: RequestLine -> BSB.Builder
encodeRequestLine (RequestLine method target version) =
    encodeMethod method <> A.fromCharList [Space]
    <> encodeRequestTarget target <> A.fromCharList [Space]
    <> encodeVersion version <> encodeLineEnd
    

encodeMethod :: Method -> BSB.Builder
encodeMethod Get = BSB.byteString [A.string|GET|] 

encodeRequestTarget :: RequestTarget -> BSB.Builder
encodeRequestTarget targetByteString = BSB.byteString $ A.lift targetByteString

-- common encoding
encodeVersion :: Version -> BSB.Builder
encodeVersion (Version x y) = [A.string|HTTP/|] <> A.fromDigitList [x] <> [A.string|.|] <> A.fromDigitList [y]

encodeField :: Field -> BSB.Builder
encodeField (Field fieldName fieldValue) = 
    (BSB.byteString $ A.lift fieldName) <> A.fromCharList [Colon, Space] <> (BSB.byteString $ A.lift fieldValue)

encodeBody :: Body -> BSB.Builder
encodeBody body = BSB.lazyByteString body

repeatedlyEncode :: (a -> BSB.Builder) -> [a] -> BSB.Builder
repeatedlyEncode = foldMap 

optionallyEncode :: (a -> BSB.Builder) -> Maybe a -> BSB.Builder
optionallyEncode = foldMap

encodeLineEnd :: BSB.Builder
encodeLineEnd = A.fromCharList crlf

crlf :: [A.Char]
crlf = [CarriageReturn, LineFeed]
