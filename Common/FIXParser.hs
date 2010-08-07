module Common.FIXParser 
	where

import Prelude hiding ( take, null, head, tail )
import Common.FIXMessage
import Common.FIXParserCombinators
import Data.Attoparsec hiding ( takeWhile1 )
import Data.Char
import Data.ByteString hiding ( pack, putStrLn, take )
import Data.ByteString.Char8 ( pack, readInt )
import Control.Applicative ( (<$>) )

-- Lookup the parser for a given FIX tag.
tagParser :: Parser FIXValue
tagParser = do l <- to_tag 	
	       v <- getParser $ toEnum l
               return v
               
tagParser' :: Parser (FixTag, FIXValue)
tagParser' = do l <- to_tag 	
	        v <- getParser $ toEnum l
                return (toEnum l,v)

-- TODO: complete mapping to parsers
getParser :: FixTag -> Parser FIXValue
getParser t =
	case t of 
		FIX_VERSION -> toFIXString
		FIX_MSG_LENGTH -> toFIXInt
		FIX_CHECKSUM -> toFIXInt
		_ -> undefined

-- Parse header and return checksum and length.
-- A header always starts with the version tag (8)  
-- followed by the length tag (9). Note: these 2 tage
-- are included int the checksum
headerParser :: Parser (Int, Int)
headerParser = do c1 <- checksum <$> (string $ pack "8=")
                  c2 <- checksum <$> to_string 
                  c3 <- checksum <$> (string $ pack "9=")
	          l <- to_string
                  let c4 = checksum l
                  let c = (c1+c2+c3+c4) `mod` 256
		  return (c, toInteger' l)

-- Parse a FIX message. The parser continues with the next
-- message when the checksum validation fails.

type PayLoad = ByteString

messageParser :: Parser PayLoad
messageParser = do (hchecksum, len) <- headerParser
		   msg <- take $ len + 1 -- FIXME: why +1?
                   c <- tagParser
                   case c of
			FIXInt i -> if (hchecksum + checksum msg) `mod` 256 == i then return msg else messageParser -- FIXME: error message instead of continuation
                        _ -> undefined

-- messagesParser :: Parser [PayLoad]
-- messagesParser = 

-- FIXME: loop not lazy
bodyParser :: Parser [(FixTag, FIXValue)]
bodyParser = do 
  x <- tagParser'
  xs <- bodyParser
  return (x:xs)
  

-- FIX checksum is simply the sum of bytes modulo 256
checksum :: ByteString -> Int
checksum b | null b = 0
           | otherwise = (fromIntegral (head b) + checksum (tail b)) `mod` 256       
