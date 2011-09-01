{-# LANGUAGE MagicHash, GeneralizedNewtypeDeriving #-}

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
    , arbibtraryFIXGroup
    , checksum
    , checksum'
	) where

import System.Time ( CalendarTime (..) )
import Prelude hiding ( take, null, head, tail, length )
import Data.Word ( Word8 )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B ( null, foldr )
import Data.IntMap ( IntMap )
import Data.Map ( Map )
import Data.ByteString.Char8 as C ( pack, cons, append )
import Data.Attoparsec ( Parser ) 
import Data.LookupTable ( LookupTable )
import qualified Data.LookupTable as LT ( insert, toList, fromList )
import Control.DeepSeq ( NFData (..) )
import Test.QuickCheck ( Gen, arbitrary, Arbitrary )
import Data.Functor ( (<$>) )
import Control.Monad ( liftM )

data FIXTag = FIXTag 
    { tName :: String
    , tnum :: Int
    , tparser :: Parser FIXValue 
    , arbitraryValue :: Gen FIXValue } 

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

--- should be alias of type in the typeclass LookupTable
newtype ListOfValues a = LoV (IntMap a) 
    deriving (LookupTable Int a, NFData)

instance Show a => Show (ListOfValues a) where
    show a = concatMap printKV $ LT.toList a
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

newtype ListOfTags a = LoT (IntMap a)
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

arbibtraryFIXValues :: FIXTags -> Gen FIXValues
arbibtraryFIXValues tags = 
    let tlist :: [FIXTag]
        tlist = map snd $ LT.toList tags
        arb :: FIXTag -> Gen (Int, FIXValue)
        arb tag = arbitraryValue tag >>= (return . ( (,) (tnum tag)))
    in
        liftM LT.fromList $ mapM arb tlist

arbibtraryFIXGroup :: FIXGroupSpec -> Gen FIXValue
arbibtraryFIXGroup spec = 
    let ltag = gsLength spec 
        stag = gsSeperator spec
        btags = gsBody spec 
    in  
        do FIXInt l <- arbitraryValue ltag 
           s <- arbitraryValue stag
           body <- (LT.insert (tnum stag) s) <$> arbibtraryFIXValues btags 
           return $ FIXGroup l [body]


instance NFData ByteString 
instance Arbitrary ByteString where
        arbitrary = C.pack <$> arbitrary 

instance NFData CalendarTime
instance Arbitrary CalendarTime where
        arbitrary = return CalendarTime {
               ctYear  = 0
             , ctMonth = toEnum 0
             , ctDay   = 0
             , ctHour  = 0
             , ctMin   = 0 
             , ctSec   = 0
             , ctPicosec = 0
             , ctWDay  = toEnum 0
             , ctYDay  = toEnum 0
             , ctTZName = "UTC"
             , ctTZ    = 0
             , ctIsDST = True }

instance Control.DeepSeq.NFData FIXValue where
    rnf (FIXInt x) = rnf x
    rnf (FIXDayOfMonth x) = rnf x
    rnf (FIXFloat x) = rnf x
    rnf (FIXQuantity x) = rnf x
    rnf (FIXPrice x) = rnf x
    rnf (FIXPriceOffset x) = rnf x
    rnf (FIXAmt x) = rnf x
    rnf (FIXChar x) = rnf x
    rnf (FIXBool x) = rnf x
    rnf (FIXString x) = rnf x
    rnf (FIXMultipleValueString x) = rnf x
    rnf (FIXCurrency x) = rnf x
    rnf (FIXExchange x) = rnf x
    rnf (FIXUTCTimestamp x) = rnf x
    rnf (FIXUTCTimeOnly x) = rnf x
    rnf (FIXLocalMktDate x) = rnf x
    rnf (FIXUTCDate x) = rnf x
    rnf (FIXMonthYear x) = rnf x
    rnf (FIXData x) = rnf x 
    rnf (FIXDataLen x) = rnf x 
    rnf (FIXGroup l vs) = rnf l `seq` rnf vs

instance Control.DeepSeq.NFData FIXMessage where
    rnf (FIXMessage h b t) = rnf h `seq` rnf b `seq` rnf t

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
    show (FIXData a) = show a
    show (FIXDataLen a) = show a
    show (FIXGroup _ _) = "group"

