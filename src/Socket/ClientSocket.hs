-- Module  : Socket.ClientSocket
-- License : LGPL-2.1 

module Socket.ClientSocket 
	where

-- Note: Don't do lazy ByteString for reading from the socket with attoparsec, see
-- http://www.serpentine.com/blog/2010/03/03/whats-in-a-parser-attoparsec-rewired-2
-- bos: "If Attoparsec has insufficient data to return a complete result, it hands back a 
-- continuation that you provide extra data to. This eliminates the need for lazy I/O 
-- and any additional buffering, and makes for a beautiful, pure API that doesn't care 
-- what its input source is."

import Network
import System.IO
import Data.ByteString hiding ( head )
import Network.Socket hiding ( recv )
import Network.Socket.ByteString

data Configuration = Configuration {
          server :: String
        , port :: Int
	} deriving (Show, Eq)

-- TODO: use scatter/gather io for sending
client :: Configuration -> (IO ByteString -> ByteString -> IO ByteString) -> IO ()
client config process = withSocketsDo $
     do addrinfos <- getAddrInfo Nothing (Just $ server config) (Just $ show (port config))
        let serveraddr = head addrinfos
        sock <- socket (addrFamily serveraddr) Stream defaultProtocol
        connect sock (addrAddress serveraddr)
        consume sock empty
        sClose sock
          where consume sock rest = process (recv sock 1024) rest >>= consume sock
