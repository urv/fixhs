module Common.FIXParser 
	( messageParser
	, bodyParser
    , nextFIXMessage
    , toFIXInt
    , toFIXChar
    , toFIXString
    , toFIXDayOfMonth
    , toFIXFloat
    , toFIXQuantity
    , toFIXPrice
    , toFIXPriceOffset
    , toFIXAmt
    , toFIXBool
    , toFIXMultipleValueString
    , toFIXCurrency
    , toFIXExchange
    , toFIXUTCTimestamp
    , toFIXUTCTimeOnly
    , toFIXLocalMktDate
	) where

import Prelude hiding ( take, null, head, tail )
import Common.FIXMessage ( FIXMessage, FIXTag(..), FIXValue(..) )
import qualified Common.FIXMessage as FIX ( delimiter, checksum )
import Common.FIXParserCombinators
import Data.Attoparsec hiding ( takeWhile1 )
import Data.Char 
import Data.ByteString hiding ( pack, take )
import Data.ByteString.Char8 ( pack )
import Data.IntMap ( IntMap )
import Data.Maybe ( fromMaybe )
import qualified Data.LookupTable as LT
import Control.Applicative ( (<$>) )
import Control.Monad ( liftM )

-- Lookup the parser for a given FIX tag.
parseFIXTag :: Parser (Int, FIXValue)
parseFIXTag = do 
    l <- toTag
    v <- tparser $ toFIXTag l
    return (l, v )


-- Parse a FIX message. The parser fails when the checksum 
-- validation fails.
messageParser :: Parser ByteString
messageParser = do 
    (hchksum, len) <- parseFIXHeader
    msg <- take len 
    (_, c) <- parseFIXTag
    let chksum = (hchksum + FIX.checksum msg) `mod` 256 
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
            c1 <- (FIX.checksum <$> string (pack "8="))
            c2 <- (FIX.checksum <$> toString)
            c3 <- (FIX.checksum <$> string (pack "9="))
            l <- toString
            let c4 = FIX.checksum l
                c = (c1 + c2 + c3 + c4 + 2 * ord FIX.delimiter) `mod` 256
            return (c, toInt' l)

-- Parse tags in the FIX body
-- Why does it return Partial? 
--  since the parser doesn't know if there is any input coming to consume
--  you have to use parseOnly instead.
bodyParser :: Parser FIXMessage
bodyParser = liftM LT.fromList $ many parseFIXTag



 -- Parse a FIX message out of the stream
nextFIXMessage :: Parser FIXMessage
nextFIXMessage = do 
    b <- messageParser -- extract the body of the FIX message
    case parseOnly bodyParser b of
        Right ts -> return ts
        Left err -> fail err


toFIXInt :: Parser FIXValue
toFIXInt = FIXInt <$> toInt

toFIXDayOfMonth :: Parser FIXValue
toFIXDayOfMonth = FIXDayOfMonth <$> toInt

toFIXFloat :: Parser FIXValue
toFIXFloat = FIXFloat <$> toFloat

toFIXQuantity :: Parser FIXValue
toFIXQuantity = FIXQuantity <$> toFloat

toFIXPrice :: Parser FIXValue
toFIXPrice = FIXPrice <$> toFloat

toFIXPriceOffset :: Parser FIXValue
toFIXPriceOffset = FIXPriceOffset <$> toFloat

toFIXAmt :: Parser FIXValue
toFIXAmt = FIXAmt <$> toFloat

toFIXBool :: Parser FIXValue
toFIXBool = FIXBool <$> toBool

toFIXString :: Parser FIXValue
toFIXString = FIXString <$> toString

toFIXMultipleValueString :: Parser FIXValue
toFIXMultipleValueString = FIXMultipleValueString <$> toString

toFIXCurrency :: Parser FIXValue
toFIXCurrency = FIXCurrency <$> toString

toFIXExchange :: Parser FIXValue
toFIXExchange = FIXExchange <$> toString

toFIXUTCTimestamp :: Parser FIXValue
toFIXUTCTimestamp = FIXUTCTimestamp <$> toUTCTimestamp

toFIXUTCTimeOnly :: Parser FIXValue
toFIXUTCTimeOnly = FIXUTCTimestamp <$> toUTCTimeOnly

toFIXLocalMktDate :: Parser FIXValue
toFIXLocalMktDate = FIXLocalMktDate <$> toLocalMktDate

toFIXChar :: Parser FIXValue
toFIXChar = FIXChar <$> toChar

dummyTag :: FIXTag
dummyTag = FIXTag 12 toFIXString

tagLookupTable :: IntMap FIXTag
tagLookupTable = LT.insert 8 (FIXTag 8  toFIXString) $ 
                 LT.insert 9 (FIXTag 9  toFIXInt) $ 
                 LT.insert 52 (FIXTag 52  toFIXUTCTimestamp) $ 
                 LT.insert 10 (FIXTag 10 toFIXInt) LT.new     

toFIXTag :: Int -> FIXTag
toFIXTag i = fromMaybe dummyTag (LT.lookup i tagLookupTable)
