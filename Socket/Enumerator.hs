module Socket.Enumerator 
	where

import Prelude hiding ( length, null )
import Network
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B ( null, empty )
import qualified Network.Socket as N hiding ( recv ) 
import Network.Socket.ByteString ( recv )
import Data.Monoid ( mappend )
import Data.Enumerator ( Enumerator, ($$) )
import qualified Data.Enumerator as E
    ( Step (..)
    , Stream (..)
    , Enumerator
    , runIteratee
    , Iteratee (..) )
import Data.Attoparsec ( Parser )
import Data.Attoparsec.Enumerator ( iterParser )

enumSocket :: Socket -> Enumerator ByteString IO a
enumSocket sock = E.Iteratee . loop where
	loop (E.Continue k) = getChunck >>= \bytes -> if B.null bytes
		then return $ E.Continue k
		else E.runIteratee (k (E.Chunks [bytes])) >>= loop
	loop step = return step
	getChunck = recv sock 1024	

-- copied from snap
enumBS :: (Monad m) => ByteString -> Enumerator ByteString m a
enumBS bs (E.Continue k) = k (E.Chunks [bs])
enumBS bs (E.Yield x s)  = E.Iteratee $ 
    return $ E.Yield x (s `mappend` E.Chunks [bs])
enumBS _  (E.Error e)    = E.Iteratee $ return $ E.Error e

data Configuration = Configuration 
    { server :: String
    , port :: Int } 
    deriving (Show, Eq)

client :: Configuration -> Parser ByteString -> IO ()
client config parser = 
    withSocketsDo $ do 
        addrinfos <- N.getAddrInfo Nothing (Just $ server config) (Just $ show (port config))
        let serveraddr = head addrinfos
        sock <- N.socket (N.addrFamily serveraddr) N.Stream N.defaultProtocol
        N.connect sock (N.addrAddress serveraddr)
        parse sock B.empty >>= consume sock
        sClose sock
         -- where parse sock rest = E.runIteratee (enumBS rest $$ enumSocket sock $$ iterParser parser)
            where 
                parse sock rest = E.runIteratee (enumSocket sock $$ enumBS rest $$ iterParser parser)
                consume sock step = case step of
                        E.Error err -> putStrLn $ "Error: " ++ show err
                        E.Yield msg rest -> print msg >> case rest of
                                                E.Chunks [m] -> parse sock m >>= consume sock
                                                E.Chunks [] -> parse sock B.empty >>= consume sock
                                                E.EOF -> putStrLn "Done"
                        E.Continue _ -> putStrLn "Continue: not expected"
