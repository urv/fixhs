import Socket.ClientSocket as C
import Socket.Enumerator as E
import Data.FIX.Parser
import qualified Data.LookupTable as LT
import Data.ByteString hiding (putStr)
import qualified Data.ByteString as B hiding ( putStr )
import qualified Data.ByteString.Char8 as C
-- import Control.Monad.Trans
import Control.Concurrent
import Data.FIX.Spec.FIX42
import Control.Monad ( liftM )
import Data.Maybe ( fromMaybe )
import Data.FIX.Message (FIXValue(..), mBody)


main :: IO ()
main = let config = E.Configuration "127.0.0.1" 3000 in 
    {-E.client config (liftM p (messageP fix42))-}
    E.client config (liftM p _nextP)
        where
            p m = C.pack $ show m
                  
                  


{-main2 :: IO ()-}
-- main = client (mapM_ print) >> putStr "done"
-- TODO: instead of writing to a channel, consider writing to 
-- a Writer Monad
{-main2 = do c <- newChan-}
           {-cs <- getChanContents c-}
           {-forkIO (process cs)-}
	   {-C.client config (parse' c)-}
	   {-where process = (mapM_ $ print . parseMessageBody)-}
			 {-parseMessageBody i = case parse bodyParser i of-}
					 {-Partial f -> f empty-}

		 {-config = C.Configuration "127.0.0.1" 3000-}
                                          
-- TODO: do partial parsing, i.e, combine chunks if they exceed the buffer size 
-- Use attoparsec-iteratee, see also:
-- http://okmij.org/ftp/Haskell/Iteratee/IterateeIO-talk-notes.pdf
{-parse' :: Chan B.ByteString -> IO B.ByteString -> B.ByteString -> IO B.ByteString-}
{-parse' c refill rest = do result <- parseWith refill messageP rest-}
                          {-case result of-}
                             {-Done rest r -> writeChan c r >> return rest-}
				 {-Partial f -> fail "partial"-}
				 {-Fail a b c -> fail $ show a ++ show c-}
                             -- FIXME: improve error handling
