module Common.FIXParser 
	( messageParser
	, bodyParser
    , headerParser
    , tagParser
	) where

import Prelude hiding ( take, null, head, tail )
import Common.FIXMessage
import Common.FIXParserCombinators
import Data.Attoparsec hiding ( takeWhile1 )
import Data.Char 
import Data.ByteString hiding ( pack, take )
import Data.ByteString.Char8 ( pack, readInt )
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad (liftM, MonadPlus(..))
import Common.FIXTag

import qualified Data.Map as M 

-- Lookup the parser for a given FIX tag.
tagParser :: Parser (FIXTag, FIXValue)
tagParser = do 
    l <- toTag
    v <- tparser $ toFIXTag l
    return (toFIXTag l, v )

-- Parse header and return checksum and length.
-- A header always starts with the version tag (8)  
-- followed by the length tag (9). Note: these 2 tags
-- are included in the checksum
headerParser :: Parser (Int, Int)
headerParser = do 
    c1 <- (checksum <$> string (pack "8="))
    c2 <- (checksum <$> toString)
    c3 <- (checksum <$> string (pack "9="))
    l <- toString
    let c4 = checksum l
        c = (c1 + c2 + c3 + c4 + 2 * ord fixDelimiter) `mod` 256
    return (c, toInt' l)

-- Parse a FIX message. The parser fails when the checksum 
-- validation fails.
messageParser :: Parser ByteString
messageParser = do 
    (hchecksum, len) <- headerParser
    msg <- take len 
    c <- tagParser
    case snd c of
        FIXInt i -> if (hchecksum + checksum msg) `mod` 256 == i 
            then return msg 
            else fail "checksum not valid"
        _        -> fail "illegal state"

-- Parse tags in the FIX body
-- Why does it return Partial? 
--  since the parser doesn't know if there is any input coming to consume
--  you have to use parseOnly instead.
bodyParser :: Parser FIXMessage
bodyParser = many tagParser
