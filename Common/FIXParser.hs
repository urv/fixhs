module Common.FIXParser 
	( messageParser
	, bodyParser
    , nextFIXMessage
	) where

import Prelude hiding ( take, null, head, tail )
import Common.FIXMessage
import Common.FIXParserCombinators
import Data.Attoparsec hiding ( takeWhile1 )
import Data.Char 
import Data.ByteString hiding ( pack, take )
import Data.ByteString.Char8 ( pack )
import Control.Applicative ( (<$>) )
import Common.FIXTag

-- Lookup the parser for a given FIX tag.
parseFIXTag :: Parser (FIXTag, FIXValue)
parseFIXTag = do 
    l <- toTag
    v <- tparser $ toFIXTag l
    return (toFIXTag l, v )


-- Parse a FIX message. The parser fails when the checksum 
-- validation fails.
messageParser :: Parser ByteString
messageParser = do 
    (hchksum, len) <- parseFIXHeader
    msg <- take len 
    (_, c) <- parseFIXTag
    let chksum = (hchksum + checksum msg) `mod` 256 
    case c of 
        FIXInt i -> if chksum == i then return msg else fail "checksum not valid"
        _        -> fail "illegal state"

    where
        -- Parse header and return checksum and length.
        -- A header always starts with the version tag (8)  
        -- followed by the length tag (9). Note: these 2 tags
        -- are included in the checksum
        parseFIXHeader :: Parser (Int, Int)
        parseFIXHeader = do 
            c1 <- (checksum <$> string (pack "8="))
            c2 <- (checksum <$> toString)
            c3 <- (checksum <$> string (pack "9="))
            l <- toString
            let c4 = checksum l
                c = (c1 + c2 + c3 + c4 + 2 * ord fixDelimiter) `mod` 256
            return (c, toInt' l)

-- Parse tags in the FIX body
-- Why does it return Partial? 
--  since the parser doesn't know if there is any input coming to consume
--  you have to use parseOnly instead.
bodyParser :: Parser FIXMessage
bodyParser = many parseFIXTag


 -- Parse a FIX message out of the stream
nextFIXMessage :: Parser FIXMessage
nextFIXMessage = do 
    b <- messageParser -- extract the body of the FIX message
    case parseOnly bodyParser b of
        Right ts -> return ts
        Left err -> fail err


