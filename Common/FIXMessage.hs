-- Module   : Common.FIXMessage
-- License  : GPLv2

{-# LANGUAGE MagicHash, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


module Common.FIXMessage 
    ( FIXValue (..)
    , FIXValues
    , FIXTag (..)
    , FIXTags
    , FIXMessages
    , FIXMessage (..)
    , FIXMessageSpec (..)
    , FIXGroupSpec (..)
    , FIXSpec (..)
    , arbibtraryFIXGroup
    , checksum
    , delimiter
    , arbitraryFIXMessage
    ) where

import System.Time ( CalendarTime (..) )
import Prelude hiding ( null )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B ( null, foldl' )
import Data.IntMap ( IntMap )
import Data.Map ( Map )
import qualified Data.ByteString.Char8 as C ( pack )
import qualified Data.Char as C ( isAscii, isAlphaNum )
import Data.Attoparsec ( Parser ) 
import Data.LookupTable ( LookupTable )
import qualified Data.LookupTable as LT ( insert, toList, fromList )
import Control.DeepSeq ( NFData (..) )
import Test.QuickCheck ( Gen, arbitrary, Arbitrary )
import Data.Functor ( (<$>) )
import Control.Monad ( replicateM, liftM )
import Data.FIX.Common ( delimiter )
import Data.Coparser ( BuilderLike (..), foldl' )

data FIXTag = FIXTag 
    { tName :: String
    , tnum :: Int
    , tparser :: Parser FIXValue 
    , arbitraryValue :: Gen FIXValue } 


data FIXValue = FIXInt Int 
              | FIXFloat Float
              | FIXChar Char 
              | FIXBool Bool 
              | FIXString ByteString 
              | FIXData ByteString 
              | FIXMultipleValueString ByteString 
              | FIXTimestamp CalendarTime
              | FIXTimeOnly CalendarTime
              | FIXDateOnly CalendarTime
              | FIXMonthYear CalendarTime
              | FIXGroup Int [FIXValues]

--- should be alias of type in the typeclass LookupTable
newtype ListOfValues a = LoV (IntMap a) 
    deriving (LookupTable Int a, NFData)

instance Show a => Show (ListOfValues a) where
    show a = concatMap printKV $ LT.toList a
        where
            printKV (k, v) = show k ++ " = " ++ show v ++ "\n"

type FIXValues = ListOfValues FIXValue 
data FIXMessage a = FIXMessage
                  { mContext :: a
                  , mType    :: ByteString
                  , mHeader  :: FIXValues
                  , mBody    :: FIXValues
                  , mTrailer :: FIXValues }


newtype ListOfTags a = LoT (IntMap a)
    deriving (LookupTable Int a)

type FIXTags = ListOfTags FIXTag
data FIXMessageSpec = FMSpec 
                      { msName    :: String
                      , msType    :: ByteString
                      , msHeader  :: FIXTags
                      , msBody    :: FIXTags 
                      , msTrailer :: FIXTags }

newtype ListOfMessages a = LM (Map ByteString a)
    deriving (LookupTable ByteString a)

type FIXMessages = ListOfMessages FIXMessageSpec
data FIXSpec = FSpec 
               { fsVersion  :: String
               , fsHeader   :: FIXTags
               , fsTrailer  :: FIXTags
               , fsMessages :: FIXMessages }

data FIXGroupSpec = FGSpec
                    { gsLength    :: FIXTag
                    , gsSeperator :: FIXTag 
                    , gsBody      :: FIXTags }


-- FIX checksum is simply the sum of bytes modulo 256
checksum :: (BuilderLike t c, Enum c)  => t -> Int
checksum b = foldl' _sumUp 0 b `mod` 256
                where 
                    _sumUp :: (Enum c) => Int -> c -> Int
                    _sumUp t c = t + fromEnum c
           
arbibtraryFIXValues :: FIXTags -> Gen FIXValues
arbibtraryFIXValues tags = 
    let tlist :: [FIXTag]
        tlist = map snd $ LT.toList tags
        arb :: FIXTag -> Gen (Int, FIXValue)
        arb tag = fmap ((,) (tnum tag)) $ arbitraryValue tag 
    in
        liftM LT.fromList $ mapM arb tlist

arbibtraryFIXGroup :: FIXGroupSpec -> Gen FIXValue
arbibtraryFIXGroup spec = 
    let ltag = gsLength spec in  
        do FIXInt l' <- arbitraryValue ltag 
           let l = l' `mod` 4
           bodies <- replicateM l arbitraryGBody
           return $ FIXGroup l bodies
    where
        arbitraryGBody = 
           let stag = gsSeperator spec
               btags = gsBody spec 
           in
               arbitraryValue stag >>=
               (\s -> LT.insert (tnum stag) s <$> arbibtraryFIXValues btags )

arbitraryFIXMessage :: FIXSpec -> FIXMessageSpec -> Gen (FIXMessage FIXSpec)
arbitraryFIXMessage context spec = do
        header <- arbibtraryFIXValues $ msHeader spec
        body <- arbibtraryFIXValues $ msBody spec
        trailer <- arbibtraryFIXValues $ msTrailer spec
        return FIXMessage 
            { mContext = context
            , mType = msType spec
            , mHeader = header
            , mBody = body
            , mTrailer = trailer }


-- An arbitrary instance of ByteString. 
--- we generate a random string out of digits and numbers
--- generated string has length at least 1 and most <max>
instance Arbitrary ByteString where
        arbitrary = do
            l' <- arbitrary :: Gen Int
            let l = 1 + l' `mod` maxLen
            C.pack <$> replicateM l (aChar isAlpha')
            where
                aChar :: (Char -> Bool) -- predicate
                        -> Gen Char     -- random generator
                aChar p = do  
                    c <- arbitrary 
                    if p c then return c else aChar p

                isAlpha' c = C.isAlphaNum c && C.isAscii c
                maxLen = 15

instance Arbitrary CalendarTime where
        arbitrary = do 
            year <- aYear
            month <- aMonth
            day <- aDay
            hour <- aHour
            minute <- aMin
            sec <- aSec
            psec <- aPsec
            return CalendarTime 
             { ctYear  = year
             , ctMonth = toEnum month
             , ctDay   = day
             , ctHour  = hour
             , ctMin   = minute
             , ctSec   = sec
             , ctPicosec = psec
             , ctWDay  = toEnum 0
             , ctYDay  = toEnum 0
             , ctTZName = "UTC"
             , ctTZ    = 0
             , ctIsDST = True }
             where
                aYear  = (`mod` 10000) <$> arbitrary 
                aMonth =    (`mod` 12) <$> arbitrary
                aHour  =    (`mod` 24) <$> arbitrary
                aDay   =    (`mod` 28) <$> arbitrary
                aMin   =    (`mod` 60) <$> arbitrary
                aSec   =    (`mod` 60) <$> arbitrary
                aPsec  = (`mod` 1000000000000) <$> arbitrary


instance Control.DeepSeq.NFData ByteString 
instance Control.DeepSeq.NFData CalendarTime

instance Control.DeepSeq.NFData FIXValue where
    rnf (FIXInt x) = rnf x
    rnf (FIXFloat x) = rnf x
    rnf (FIXChar x) = rnf x
    rnf (FIXBool x) = rnf x
    rnf (FIXString x) = rnf x
    rnf (FIXMultipleValueString x) = rnf x
    rnf (FIXTimestamp x) = rnf x
    rnf (FIXTimeOnly x) = rnf x
    rnf (FIXDateOnly x) = rnf x
    rnf (FIXMonthYear x) = rnf x
    rnf (FIXData x) = rnf x 
    rnf (FIXGroup l vs) = rnf l `seq` rnf vs

instance Control.DeepSeq.NFData (FIXMessage a) where
    rnf (FIXMessage _ _ h b t) = rnf h `seq` rnf b `seq` rnf t

