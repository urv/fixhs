module Common.FIXMessage 
    ( delimiter
    , FIXValue (..)
    , FIXValues
    , FIXTag (..)
    , FIXTags
    , fixVersion
    , FIXMessage (..)
    , FIXMessageSpec (..)
    , FIXSpec (..)
    , paddedChecksum
    , checksum
    , checksum'
	) where

import System.Time ( CalendarTime )
import Prelude hiding ( take, null, head, tail, length )
import Data.Word ( Word8 )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B ( null, head, tail, foldr )
import Data.IntMap ( IntMap )
import Data.Map ( Map )
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
              | FIXGroup FIXValues
              deriving (Show)

--- should be alias of type in the typeclass LookupTable
type FIXValues = IntMap FIXValue
data FIXMessage = FIXMessage
                  { mHeader :: FIXValues
                  , mBody :: FIXValues
                  , mTrailer :: FIXValues }
                  deriving (Show)

type FIXTags = IntMap FIXTag
data FIXMessageSpec = FMSpec 
                      { msType :: ByteString
                      , msHeader :: FIXTags
                      , msBody :: FIXTags 
                      , msTrailer :: FIXTags }

type FIXMessages = Map ByteString FIXMessageSpec
data FIXSpec = FSpec 
               { fsHeader :: FIXTags
               , fsTrailer :: FIXTags
               , fsMessages :: FIXMessages }

delimiter :: Char
delimiter = '\SOH'

fixVersion :: ByteString
fixVersion = C.pack "8=FIX.4.2"

paddedChecksum :: ByteString -> ByteString
paddedChecksum = checksum' . checksum

-- FIX checksum is simply the sum of bytes modulo 256
checksum :: ByteString -> Int
checksum b | B.null b = 0
           | otherwise = B.foldr _sumUp 0 b `mod` 256
                where 
                    _sumUp :: Word8 -> Int -> Int
                    _sumUp c = (+) (fromIntegral c)
           
-- FIX length
checksum' :: Int -> ByteString
checksum' b | b < 10 = C.pack "00" `append` num
            | b < 100 = C.cons '0' num
	        | otherwise = num
            where num = C.pack (show b)
