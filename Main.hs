import Socket.ClientSocket
import Common.FIXParser
import Data.Attoparsec 
import Data.ByteString hiding (putStr)
import qualified Data.ByteString as B hiding ( putStr )
import qualified Data.ByteString.Char8 as C
import Control.Monad.Trans
import Control.Concurrent
import Common.FIXParser

main :: IO ()
-- main = client (mapM_ print) >> putStr "done"
main = do c <- newChan
          cs <- getChanContents c
          forkIO (process cs)
	  client (parse' c)
	  where process = (mapM_ $ print . parseMessageBody)
	        parseMessageBody i = case parse bodyParser i of
					Partial f -> f empty
                                          
-- TODO: do partial parsing, i.e, combine chunks if they exceed the buffer size 
-- Use attoparsec-iteratee, see also:
-- http://okmij.org/ftp/Haskell/Iteratee/IterateeIO-talk-notes.pdf
parse' :: Chan B.ByteString -> IO B.ByteString -> B.ByteString -> IO B.ByteString
parse' c refill rest = do result <- parseWith refill messageParser rest
                          case result of
                             Done rest r -> writeChan c r >> return rest
			     Partial f -> fail "partial"
			     Fail a b c -> fail $ show a ++ show c
                             -- FIXME: improve error handling
