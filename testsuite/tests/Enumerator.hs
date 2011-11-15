import Data.Enumerator
import Data.Enumerator.IO
import Data.Attoparsec.Enumerator

import Common.FIXParser

testli = do res <- run (enumFile "Test/ExampleFixMessages.txt" $$ iterParser messageParser)
	    case res of 
		Left err -> putStrLn $ "Error: " ++ show err
		Right msg -> putStrLn $ "Done: " ++ show msg
