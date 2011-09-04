{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Common.FIXCoparser 
	(
	-- 
	coparse
	) where

import Prelude as P
import Common.FIXMessage 
    ( FIXSpec, FIXMessage (..), tnum, FIXValues, FIXValue (..) )
import Common.FIXParser ( tBeginString, tBodyLength, tCheckSum, tMsgType )
import qualified Common.FIXMessage as FIX ( checksum, delimiter )
import qualified Data.FIX.Common as FIX ( fixVersion )
import Data.Coparser ( Coparser (..), TextLike (..) )
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 as C ( unpack, length )
import qualified Data.LookupTable as LT
import System.Time ( CalendarTime (..) )

-- implementation is not efficient 
-- this is just meant for testing 
-- and needs a lot of cleanup
--
-- ideas and issues:
-- a) maybe use some kind of Monoid, resp. Writer, Builder to put together
--    the ByteString. Maybe blazer-builder or Data.Text.Lazy.Builder Monoid
-- b) lazy vs. strict ByteString?
-- c) as for parsing we use a Monad, can we use the dual, a Comonad, here?
-- d) implement Binary for FIXMessage, then call encode to get the ByteString
--    -> seems to be slow (see comments in blaze-builder). also we just need
--       one direction, i.e. put - get would be the FIXParser


instance Coparser FIXValues where
    coparse = pack . _serialize . LT.toList  
        where
            _serialize :: [(Int,FIXValue)] -> String
            _serialize = P.concatMap _serValue
                where
                    _serValue :: (Int, FIXValue) -> String
                    _serValue (k, FIXGroup i ls) = 
                        let sub = P.concatMap (_serialize . LT.toList) ls 
                            delim = FIX.delimiter         
                        in
                            show k ++ '=' : show i ++ delim : sub
                    _serValue (k, v) = 
                        let delim = FIX.delimiter in
                            show k ++ '=' : show v ++ [delim] 


-- externalize the FIXMessage
instance Coparser (FIXMessage FIXSpec) where
    coparse m = pack . C.unpack $ msg' `append` chksum' 
        where 
            msg' = header 
                `append` len' `snoc` FIX.delimiter 
                `append` body'

            chksum' = ctag `append` equals `append` paddedChecksum msg' `snoc` FIX.delimiter
            len' = ltag `append` equals `append` pack (show $ C.length body')
            mtype' = mtag `append` equals `append` pack (C.unpack $ mType m)
            body' = mtype' `snoc` FIX.delimiter 
                `append` coparse (mHeader m) 
                `append` coparse (mBody m) 
                `append` coparse (mTrailer m)
            
            ctag = pack . show $ tnum tCheckSum
            ltag = pack . show $ tnum tBodyLength 
            mtag = pack . show $ tnum tMsgType

            equals = singleton '='
            header = pack (C.unpack FIX.fixVersion) `snoc` FIX.delimiter -- FIX Header

            paddedChecksum = pack . pad 3 . FIX.checksum


fromFIXMonthYear :: CalendarTime -> String
fromFIXMonthYear c = 
    let year = ctYear c; month = 1 + fromEnum (ctMonth c) in
        pad 4 year ++ pad 2 month

fromFIXUTCData :: CalendarTime -> String
fromFIXUTCData c = let day = 1 + ctDay c in
    fromFIXMonthYear c ++ pad 2 day 

fromFIXLocalMktDate :: CalendarTime -> String
fromFIXLocalMktDate = fromFIXUTCData

fromFIXUTCTimeOnly :: CalendarTime -> String
fromFIXUTCTimeOnly c = 
    let min = ctMin c; sec = ctSec c; hours = ctHour c in
        pad 2 hours ++ ':' : pad 2 min ++ ':' : pad 2 sec

fromFIXUTCTimetamp :: CalendarTime -> String
fromFIXUTCTimetamp c =  fromFIXUTCData c ++ '-' : 
    fromFIXUTCTimeOnly c


instance Show FIXValue where
    show (FIXInt a) = show a
    show (FIXDayOfMonth a) = show a
    show (FIXFloat a) = show a
    show (FIXQuantity a) = show a
    show (FIXPrice a) = show a
    show (FIXPriceOffset a) = show a
    show (FIXAmt a) = show a
    show (FIXChar a) = [a]
    show (FIXBool a) 
        | a = "Y"
        | otherwise = "N"
    show (FIXString a) = C.unpack a
    show (FIXMultipleValueString a) = C.unpack a
    show (FIXCurrency a) = C.unpack a
    show (FIXExchange a) = C.unpack a
    show (FIXUTCTimestamp a) = fromFIXUTCTimetamp a
    show (FIXUTCTimeOnly a) = fromFIXUTCTimeOnly a
    show (FIXLocalMktDate a) = fromFIXLocalMktDate a
    show (FIXUTCDate a) = fromFIXUTCData a
    show (FIXMonthYear a) = fromFIXMonthYear a
    show (FIXData a) = C.unpack a
    show (FIXDataLen a) = show a
    show (FIXGroup _ ls) = show ls


instance Show (FIXMessage a) where
    show m = show (mHeader m) 
        ++ '\n' : show (mBody m) ++ '\n' : show (mTrailer m) 

pad :: Int -> Int -> String
pad w i | d <= 0 = t
        | d == 1 = '0' : t
        | otherwise = let prefix = P.replicate d '0' in prefix ++ t
    where
        t = show i
        d = w - P.length t
