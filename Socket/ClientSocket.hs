module Socket.ClientSocket 
	where

-- Note: Don't do lazy ByteString for reading from the socket with attoparsec, see
-- http://www.serpentine.com/blog/2010/03/03/whats-in-a-parser-attoparsec-rewired-2
-- bos: "If Attoparsec has insufficient data to return a complete result, it hands back a 
-- continuation that you provide extra data to. This eliminates the need for lazy I/O 
-- and any additional buffering, and makes for a beautiful, pure API that doesn't care 
-- what its input source is."

import Data.Attoparsec
import qualified Data.ByteString as B hiding ( putStr )
import Network
import System.IO

import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString

import Control.Monad.Trans
import Common.FIXParser

import Control.Concurrent

server :: String
server = "127.0.0.1"

port :: Int
port = 3000

-- TODO: do partial parsing, i.e, combine chunks if they exceed the buffer size 
-- Use attoparsec-iteratee, see also:
-- http://okmij.org/ftp/Haskell/Iteratee/IterateeIO-talk-notes.pdf
parse' :: IO B.ByteString -> Chan B.ByteString -> B.ByteString -> IO B.ByteString
parse' refill c rest = do result <- parseWith refill messageParser rest
                          case result of
                             Done rest r -> writeChan c r >> return rest
                             _ -> undefined -- FIXME: error handling
                                          
client :: ([B.ByteString] -> IO ()) -> IO ()
client process = withSocketsDo $
     do addrinfos <- getAddrInfo Nothing (Just server) (Just $ show port)
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        c <- newChan
        cs <- getChanContents c
        forkIO (process cs)
        consume sock c B.empty
        sClose sock
          where consume sock c rest = parse' (recv sock 1024) c rest >>= consume sock c