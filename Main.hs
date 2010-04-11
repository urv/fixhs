import Socket.ClientSocket

main :: IO ()
main = client (mapM_ print) >> putStr "done"

