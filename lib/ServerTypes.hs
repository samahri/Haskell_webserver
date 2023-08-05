module ServerTypes where

import ASCII (ASCII)
import Data.ByteString (StrictByteString)
import ASCII.Decimal (Digit (..))
import Data.ByteString.Lazy (LazyByteString)
import Data.Maybe (Maybe)
import Text.Show (Show)

data Request = Request RequestLine [Field] (Maybe Body)
data Response = Response StatusLine [Field] (Maybe Body)

-- Request related types
data RequestLine = RequestLine Method RequestTarget Version deriving Show
data Method = Get deriving Show
type RequestTarget = ASCII StrictByteString 

-- Response Related types
data StatusLine = StatusLine Version StatusCode (Maybe ReasonPhrase)
data StatusCode = StatusCode Digit Digit Digit -- Need to find a way to only limit it to valid status code
type ReasonPhrase = ASCII StrictByteString

-- Common types
data Version = Version Digit Digit deriving Show

data Field = Field FieldName FieldValue
type FieldName = ASCII StrictByteString
type FieldValue = ASCII StrictByteString

type Body = LazyByteString
