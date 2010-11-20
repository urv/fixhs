import Socket.ClientSocket
import Common.FIXParser

main :: IO ()
-- TODO: replace print by something useful
-- main = client (mapM_ print) >> putStr "done"
-- FIXME: parseMessage doesn't work 
-- main = client (mapM_ $ print . parseMessage)
main = client (mapM_ $ print . parseMessageBody)
