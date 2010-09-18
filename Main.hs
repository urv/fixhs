import Socket.ClientSocket
import Common.FIXParser
import Common.FIXMessage
import Data.Attoparsec hiding ( takeWhile1 )
import Data.ByteString 


main :: IO ()
-- main = client (mapM_ print) >> putStr "done"
main = client (mapM_ $ print . parseMessageBody) 

parseMessageBody :: ByteString -> Result [FIXMessage]
parseMessageBody r = case parse bodyParser r of
                          Partial f -> f empty
                          _ -> undefined
