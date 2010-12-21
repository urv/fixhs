module Socket.Enumerator 
	where

import Prelude hiding ( length, null )
import Network
import System.IO
import Data.ByteString hiding ( head, putStrLn )
import Network.Socket hiding ( recv )
import Network.Socket.ByteString
import Data.Monoid ( mappend )

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

-- copied from snap
enumBS :: (Monad m) => ByteString -> Enumerator ByteString m a
enumBS bs (Continue k) = k (Chunks [bs])
enumBS bs (Yield x s)  = Iteratee $ return $ Yield x (s `mappend` Chunks [bs])
enumBS _  (Error e)    = Iteratee $ return $ Error e

data Configuration = Configuration {
          server :: String
        , port :: Int
	} deriving (Show, Eq)

client :: Configuration -> Parser ByteString -> IO ()
client config parser = withSocketsDo $
     do addrinfos <- getAddrInfo Nothing (Just $ server config) (Just $ show (port config))
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        parse sock empty >>= consume sock
        sClose sock
        where parse sock rest = runIteratee (enumBS rest $$ enumSocket sock $$ iterParser parser)
	      consume sock step = case step of
	    			    Error err -> putStrLn $ "Error: " ++ show err
				    Yield msg rest -> putStrLn (show msg) >> case rest of 
				    						Chunks [m] -> parse sock m >>= consume sock
										-- FIXME: what's the correct pattern...?
				    						Chunks (x:xs) -> putStrLn (show x) >> parse sock x >>= consume sock
										EOF -> putStrLn "Done"
	    			    Continue _ -> putStrLn $ "Continue: not expected"
