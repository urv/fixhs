module Common.FIXParser 
	( messageParser
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
import Common.FIXMessage ( FIXTags, FIXSpec (..), FIXMessage (..), 
    FIXTag (..), FIXValue(..), FIXMessageSpec (..), FIXValues )
import qualified Common.FIXMessage as FIX ( delimiter, checksum )
import Common.FIXParserCombinators 
import Data.Attoparsec hiding ( takeWhile1 )
import Data.Char ( ord )
import Data.ByteString hiding ( pack, take )
import Data.ByteString.Char8 ( pack )
import Data.Maybe ( fromMaybe )
import qualified Data.LookupTable as LT 
import Control.Applicative ( (<$>) )
import Control.Monad ( liftM )

-- Lookup the parser for a given FIX tag.
parseFIXTag :: FIXTags -> Parser (Int, FIXValue)
parseFIXTag tags = do 
    l <- toTag
    let tag' = LT.lookup l tags
        parser' = fromMaybe (fail "") $ liftM tparser tag' 
        in fmap ((,) l) parser' 


-- Parse a FIX message. The parser fails when the checksum 
-- validation fails.
messageParser :: Parser ByteString
messageParser = do 
    (hchksum, len) <- _getHeader
    msg <- take len 
    c <- _getChecksum
    let chksum = (hchksum + FIX.checksum msg) `mod` 256  in
        if chksum == c then return msg else fail "checksum not valid"

    where
        -- Parse header and return checksum and length.
        -- A header always starts with the version tag (8)  
        -- followed by the length tag (9). Note: these 2 tags
        -- are included in the checksum
        _getHeader :: Parser (Int, Int)
        _getHeader = do 
            c1 <- (FIX.checksum <$> string (pack "8="))
            c2 <- (FIX.checksum <$> toString)
            c3 <- (FIX.checksum <$> string (pack "9="))
            l <- toString
            let c4 = FIX.checksum l
                c = (c1 + c2 + c3 + c4 + 2 * ord FIX.delimiter) `mod` 256
            return (c, toInt' l)

        _getChecksum :: Parser Int
        _getChecksum = string (pack "10=") >> toInt

-- Parse tags in the FIX body
-- Why does it return Partial? 
--  since the parser doesn't know if there is any input coming to consume
--  you have to use parseOnly instead.
tagsP :: FIXTags -> Parser FIXValues
tagsP ts = let tag' = parseFIXTag ts in 
               liftM LT.fromList $ many tag'

 -- Parse a FIX message out of the stream
nextFIXMessage :: FIXSpec -> Parser FIXMessage
nextFIXMessage spec = 
    let headerP  = tagsP $ fsHeader spec 
        trailerP = tagsP $ fsTrailer spec 
        bodyP t = tagsP (msBody $ fromMaybe undefined (LT.lookup t (fsMessages spec)))
    in do
        h <- headerP
        let mt' = fromMaybe undefined (LT.lookup 35 h) 
            mt = case mt' of FIXString t -> t
                             _ -> undefined
        b <- bodyP mt
        t <- trailerP 
        return FIXMessage { mHeader = h, mBody = b, mTrailer = t }

nm :: FIXSpec -> Parser FIXMessage
nm spec = 
    do msg <- messageParser -- extract the body of the FIX message
       case parseOnly (nextFIXMessage spec) msg of
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
