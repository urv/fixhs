module Data.FIX.Arbitrary 
	( arbibtraryFIXValues
	, arbibtraryFIXGroup
	, arbitraryFIXMessage ) 
	where

import Common.FIXMessage ( 
	FIXGroupElement(..), FIXTag(..), FIXValue(..), FIXValues, FIXTags
      , FIXMessage(..), FIXSpec, FIXMessageSpec(..), FIXGroupSpec(..) )
import System.Time ( CalendarTime (..) )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C ( pack )
import qualified Data.Char as C ( isAscii, isAlphaNum )
import qualified Data.LookupTable as LT ( insert, toList, fromList, new )
import Data.Functor ( (<$>) )
import Control.Monad ( replicateM, liftM )
import Test.QuickCheck ( Gen, arbitrary, Arbitrary )

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
           in do
               s  <- arbitraryValue stag 
	       vs <- arbibtraryFIXValues btags
	       return (FIXGroupElement (tnum stag) s vs)

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



