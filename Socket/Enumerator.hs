module Socket.Enumerator 
	where

import Prelude hiding ( length, null )
import Network
import System.IO
import Data.ByteString hiding ( head, putStrLn )
import Network.Socket hiding ( recv )
import Network.Socket.ByteString

import Data.Enumerator hiding ( head, length )
import Data.Enumerator.IO
import Data.Attoparsec
import Data.Attoparsec.Enumerator

enumSocket :: Socket -> Enumerator ByteString IO a
enumSocket sock = Iteratee . loop where
	loop (Continue k) = getChunck >>= \bytes -> if null bytes
		then return $ Continue k
		else runIteratee (k (Chunks [bytes])) >>= loop
	loop step = return step
	getChunck = recv sock 1024	

data Configuration = Configuration {
          server :: String
        , port :: Int
	} deriving (Show, Eq)

client :: Show a => Configuration -> Parser a -> IO ()
client config parser = withSocketsDo $
     do addrinfos <- getAddrInfo Nothing (Just $ server config) (Just $ show (port config))
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        res <- run (enumSocket sock $$ iterParser parser)
	case res of
	    Left err -> putStrLn $ "Error: " ++ show err
	    Right msg -> putStrLn $ "Done: " ++ show msg
        sClose sock

