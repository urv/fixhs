import Socket.ClientSocket
import Common.FIXParser
import Data.Attoparsec 
import Data.ByteString hiding (putStr)

main :: IO ()
-- TODO: replace print by something useful
-- main = client (mapM_ print) >> putStr "done"
-- FIXME: parseMessage doesn't work 
-- main = client (mapM_ $ print . parseMessage)
main = client (mapM_ $ print . parseMessageBody)
	where parseMessageBody i = case parse bodyParser i of
					Partial f -> f empty
