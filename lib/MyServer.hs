module MyServer(runServer) where

import Relude hiding (many)
import Network.Simple.TCP (recv, serve, send, HostPreference (..), ServiceName)
import qualified System.IO as IO
import qualified Data.ByteString.Lazy as LBS
import ServerTypes
import Encoders (encodeOkResponse)
import Parsers(parseRequestLine)

runServer :: IO ()
runServer = serve @IO localhost port8000 \(s, a) -> do
        putStrLn ("New connection from " <> show a)
        headerMaybe <- recv s 100000 -- arbitrary length, for now
        case headerMaybe of
          Just headerInfo -> do
              let requestLine = parseRequestLine headerInfo 
              case requestLine of
                 Right method -> IO.putStrLn $ show method 
                 Left e -> IO.putStrLn $ show e 
          Nothing -> LBS.putStr ""
        body <- readHtmlDoc
        send s $ encodeOkResponse body 


readHtmlDoc:: IO Body
readHtmlDoc = do
        handle <- IO.openFile htmlFilePath ReadMode
        body <- LBS.hGetContents handle
        return body

htmlFilePath :: FilePath
htmlFilePath = "html/index.html"

localhost :: HostPreference
localhost = Host "127.0.0.1"

port8000 :: ServiceName
port8000 = "8000"
