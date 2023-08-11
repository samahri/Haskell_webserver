module MyServer(runServer) where

import Relude hiding (many)
import Network.Simple.TCP (recv, serve, send, HostPreference (..), ServiceName)
import qualified System.IO as IO
import qualified Data.ByteString.Lazy as LBS
import ServerTypes
import Encoders (encodeOkResponse, encodeBadRequestResponse)
import Parsers(parseRequestLine)
import ASCII.Refinement as AR

runServer :: IO ()
runServer = serve @IO localhost port8000 \(s, a) -> do
        putStrLn ("New connection from " <> show a)
        headerMaybe <- recv s 100000 -- arbitrary length, for now
        case headerMaybe of
          Just headerInfo -> do
              IO.putStrLn $ show headerInfo
              let requestHeader = parseRequestLine headerInfo 
              case requestHeader of
                 Right requestHeaderInfo -> do
                     IO.putStrLn $ show requestHeaderInfo 
                     let bodyPath = getPathFromRequest requestHeaderInfo
                     body <- readHtmlDoc bodyPath
                     send s $ encodeOkResponse body 
                 Left e -> do
                     IO.putStrLn $ show e 
                     send s encodeBadRequestResponse
          Nothing -> LBS.putStr ""

readHtmlDoc:: RequestTarget -> IO Body
readHtmlDoc path = do
        let requestPath = htmlFilePath path
        handle <- IO.openFile requestPath ReadMode
        body <- LBS.hGetContents handle
        return body

getPathFromRequest :: Request -> RequestTarget
getPathFromRequest (Request (RequestLine _ target _) _ _) = target 

htmlFilePath :: RequestTarget -> FilePath
htmlFilePath path = "html" <> (decodeUtf8 $ AR.lift path) <> "index.html"

localhost :: HostPreference
localhost = Host "127.0.0.1"

port8000 :: ServiceName
port8000 = "8000"

requestLineLength :: Int
requestLineLength = 8000 

