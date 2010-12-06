import Socket.ClientSocket
import Common.FIXParser
import Data.Attoparsec 
import Data.ByteString hiding (putStr)

main :: IO ()
-- main = client (mapM_ print) >> putStr "done"
main = client (mapM_ $ print . parseMessageBody)
	where parseMessageBody i = case parse bodyParser i of
					Partial f -> f empty
