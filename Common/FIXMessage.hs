{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Common.FIXMessage 
    ( delimiter
    , FIXValue (..)
    , FIXValues
    , FIXTag (..)
    , FIXTags
    , fixVersion
    , FIXMessages
    , FIXMessage (..)
    , FIXMessageSpec (..)
    , FIXGroupSpec (..)
    , FIXSpec (..)
    , paddedChecksum
    , checksum
    , checksum'
	) where

import System.Time ( CalendarTime )
import Prelude hiding ( take, null, head, tail, length )
import Data.Word ( Word8 )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B ( null, foldr )
import Data.IntMap ( IntMap )
import Data.Map ( Map )
import Data.ByteString.Char8 as C ( pack, cons, append )
import Data.Attoparsec ( Parser ) 
import Data.LookupTable ( LookupTable )
import qualified Data.LookupTable ( toList )

data FIXTag = FIXTag 
    { tName :: String
    , tnum :: Int
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
              | FIXData ByteString 
              | FIXDataLen Int
              | FIXGroup Int [FIXValues]

instance Show FIXValue where
    show (FIXInt a) = show a
    show (FIXDayOfMonth a) = show a
    show (FIXFloat a) = show a
    show (FIXQuantity a) = show a
    show (FIXPrice a) = show a
    show (FIXPriceOffset a) = show a
    show (FIXAmt a) = show a
    show (FIXChar a) = show a
    show (FIXBool a) = show a
    show (FIXString a) = show a
    show (FIXMultipleValueString a) = show a
    show (FIXCurrency a) = show a
    show (FIXExchange a) = show a
    show (FIXUTCTimestamp _) = "time"
    show (FIXUTCTimeOnly _) = "time"
    show (FIXLocalMktDate _) = "time"
    show (FIXUTCDate _) = "time"
    show (FIXMonthYear _) = "time"
    show (FIXData _ a) = show a
    show (FIXGroup _ _) = "group"

--- should be alias of type in the typeclass LookupTable
newtype ListOfValues a = LV (IntMap a) 
    deriving (LookupTable Int a)

instance Show a => Show (ListOfValues a) where
    show a = concatMap printKV $ Data.LookupTable.toList a
        where
            printKV (k, v) = show k ++ " = " ++ show v ++ "\n"

type FIXValues = ListOfValues FIXValue 
data FIXMessage = FIXMessage
                  { mHeader :: FIXValues
                  , mBody :: FIXValues
                  , mTrailer :: FIXValues }

instance Show FIXMessage where
    show m = 
        "Header:\n\n" ++ show (mHeader m) ++ "\n"
        ++ "Body:\n\n" ++ show (mBody m) ++ "\n"
        ++ "Trailer:\n\n" ++ show (mTrailer m) 

newtype ListOfTags a = LT (IntMap a)
    deriving (LookupTable Int a)

type FIXTags = ListOfTags FIXTag
data FIXMessageSpec = FMSpec 
                      { msName :: String
                      , msType :: ByteString
                      , msHeader :: FIXTags
                      , msBody :: FIXTags 
                      , msTrailer :: FIXTags }

newtype ListOfMessages a = LM (Map ByteString a)
    deriving (LookupTable ByteString a)

type FIXMessages = ListOfMessages FIXMessageSpec
data FIXSpec = FSpec 
               { fsHeader :: FIXTags
               , fsTrailer :: FIXTags
               , fsMessages :: FIXMessages }

data FIXGroupSpec = FGSpec
                    { gsLength :: FIXTag
                    , gsSeperator :: FIXTag 
                    , gsBody :: FIXTags }

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
