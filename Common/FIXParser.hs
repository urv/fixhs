module Common.FIXParser 
	(
	messageParser
	, bodyParser
	) where

import Prelude hiding ( take, null, head, tail )
import Common.FIXMessage
import Common.FIXParserCombinators
import Data.Attoparsec hiding ( takeWhile1 )
import Data.Char 
import Data.ByteString hiding ( pack, take )
import Data.ByteString.Char8 ( pack, readInt )
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad (MonadPlus(..))

import qualified Data.Map as M 

-- Lookup the parser for a given FIX tag.
tagParser :: Parser (FIXTag, FIXValue)
tagParser = do 
    l <- to_tag
    v <- getParser $ toEnum l
    return (toEnum l,v)

getParser :: FIXTag -> Parser FIXValue
getParser t = M.findWithDefault toFIXString t parserMap

parserMap :: M.Map FIXTag (Parser FIXValue)
parserMap = M.fromList 
                [(FIX_VERSION, toFIXString), 
                 (FIX_MSG_LENGTH, toFIXInt),                  
                 (FIX_CHECKSUM, toFIXInt)] -- FIXME: complete map

-- Parse header and return checksum and length.
-- A header always starts with the version tag (8)  
-- followed by the length tag (9). Note: these 2 tags
-- are included in the checksum
headerParser :: Parser (Int, Int)
headerParser = do 
    c1 <- (checksum <$> string (pack "8="))
    c2 <- (checksum <$> to_string)
    c3 <- (checksum <$> string (pack "9="))
    l <- to_string
    let c4 = checksum l
        c = (c1 + c2 + c3 + c4 + 2 * ord fix_delimiter) `mod` 256
    return (c, toInteger' l)

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
-- FIXME: why does it return Partial? 
bodyParser :: Parser FIXMessage
bodyParser = many tagParser
