import Socket.ClientSocket
import Common.FIXParser

main :: IO ()
-- TODO: replace print by something useful
-- main = client (mapM_ print) >> putStr "done"
main = client (mapM_ $ print . parseMessage)
