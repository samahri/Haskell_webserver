module ServerTypes where

import Data.Maybe (Maybe(Just))
import ASCII (ASCII)
import qualified ASCII as A
import Data.ByteString (StrictByteString)
import ASCII.Decimal (Digit (..))
import Data.ByteString.Lazy (LazyByteString)
import Text.Show (Show)

data Request = Request RequestLine [Field] (Maybe Body) deriving Show
data Response = Response StatusLine [Field] (Maybe Body)

-- Request related types
data RequestLine = RequestLine Method RequestTarget Version deriving Show
data Method = Get deriving Show
type RequestTarget = ASCII StrictByteString 

-- Response Related types
data StatusLine = StatusLine Version StatusCode 
data StatusCode = StatusCode Code (Maybe ReasonPhrase)-- Need to find a way to only limit it to valid status codes
type ReasonPhrase = ASCII StrictByteString
type Code = (Digit,Digit,Digit) 

-- Common types
type Version = (Major, Minor) 
type Major = Digit
type Minor = Digit

data Field = Field FieldName FieldValue deriving Show
type FieldName = ASCII StrictByteString 
type FieldValue = ASCII StrictByteString

type Body = LazyByteString

-- constants
http1_1 :: Version
http1_1 = (Digit1,Digit1)

status200 :: StatusCode
status200 = StatusCode (Digit2,Digit0,Digit0) (Just [A.string|OK|]) 

status400 :: StatusCode
status400 = StatusCode (Digit4,Digit0,Digit0) (Just [A.string|Bad Request|])
