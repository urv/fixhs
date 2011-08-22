module Common.FIXMessage 
    ( delimiter
    , FIXValue (..)
    , FIXTag (..)
    , fixVersion
    , FIXMessage
    , FIXMessageSpec (..)
    , paddedChecksum
    , checksum
    , checksum'
	) where

import System.Time ( CalendarTime )
import Prelude hiding ( take, null, head, tail, length )
import Data.ByteString 
import Data.IntMap ( IntMap )
import Data.ByteString.Char8 as C hiding ( take, null, head, tail, length )
import Data.Attoparsec ( Parser )

data FIXTag = FIXTag 
    { tnum :: Int
    , tparser :: Parser FIXValue } 

instance Show FIXTag where
    show = show . tnum 

instance Eq FIXTag where
    s == t = tnum s == tnum t

data FIXValue = FIXInt Int 
              | FIXDayOfMonth Int
              | FIXFloat Float
              | FIXQuantity Float
              | FIXPrice Float
              | FIXPriceOffset Float
              | FIXAmt Float
              | FIXChar Char 
              | FIXBool Bool 
              | FIXString ByteString 
              | FIXMultipleValueString ByteString 
              | FIXCurrency ByteString 
              | FIXExchange ByteString 
              | FIXUTCTimestamp CalendarTime
              | FIXUTCTimeOnly CalendarTime
              | FIXLocalMktDate CalendarTime
              | FIXUTCDate CalendarTime
              | FIXMonthYear CalendarTime
              | FIXData 
                { dataLen :: Int
                , dataChunk :: ByteString }
              | FIXGroup FIXMessage
              deriving (Show)

--- should be alias of type in the typeclass LookupTable
type FIXValues = IntMap FIXValue
type FIXMessage = FIXValues

type FIXTags = IntMap FIXTag
data FIXMessageSpec = FMSpec 
                      { mType :: ByteString
                      , mTags :: FIXTags }

delimiter :: Char
delimiter = '\SOH'

fixVersion :: ByteString
fixVersion = C.pack "8=FIX.4.2"

paddedChecksum :: ByteString -> ByteString
paddedChecksum = checksum' . checksum

-- FIX checksum is simply the sum of bytes modulo 256
checksum :: ByteString -> Int
checksum b | null b = 0
           | otherwise = (fromIntegral (head b) + checksum (tail b)) `mod` 256       

-- FIX length
checksum' :: Int -> ByteString
checksum' b | b < 10 = C.pack "00" `append` num
            | b < 100 = C.cons '0' num
	        | otherwise = num
            where num = C.pack (show b)
