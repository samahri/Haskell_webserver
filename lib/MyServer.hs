module MyServer(runServer) where

import Relude
import Network.Simple.TCP (serve, send, HostPreference (..), ServiceName)
import qualified ASCII as A
import qualified System.IO as IO
import qualified Data.ByteString.Lazy as LBS
import ServerTypes
import Encoders (encodeOkResponse)

runServer :: IO ()
runServer = serve @IO localhost port8000 \(s, a) -> do
        putStrLn ("New connection from " <> show a)
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
