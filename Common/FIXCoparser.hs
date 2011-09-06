{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Common.FIXCoparser 
	(
	-- 
	coparse
	) where

import Prelude as P hiding (concat)
import Common.FIXMessage 
    ( FIXSpec, FIXMessage (..), tnum, FIXValues, FIXValue (..) )
import Common.FIXParser ( tBeginString, tBodyLength, tCheckSum, tMsgType )
import qualified Common.FIXMessage as FIX ( checksum, delimiter )
import qualified Data.FIX.Common as FIX ( fixVersion )
import Data.Coparser ( Coparser (..), BuilderLike, 
    pack, concat, append, cons, snoc, singleton, decimal, realFloat )
import qualified Data.Coparser as Text ( length )
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
            _serialize = concat . (P.map _serValue)
                where
                    _serValue (k, FIXGroup i ls) = 
                        let sub = concat $ P.map (_serialize . LT.toList) ls 
                            delim = FIX.delimiter         
                        in
                            pack (show k) `append` ('=' `cons` pack (show i) `append` (delim `cons` sub))
                    _serValue (k, v) = 
                        let delim = FIX.delimiter in
                            pack (show k) `append` ('=' `cons` coparse v `append` singleton delim)



-- externalize the FIXMessage
instance Coparser (FIXMessage FIXSpec) where
    coparse m = msg' `append` chksum' 
        where 
            msg' = header `append` (len' `append` (FIX.delimiter `cons` body'))

            chksum' = ctag `append` ('=' `cons` (paddedChecksum msg' `snoc` FIX.delimiter))
            len' = ltag `append` ('=' `cons` pack (show $ Text.length body'))
            mtype' = mtag `append` ('=' `cons` pack (C.unpack $ mType m))
            body' = mtype' 
                `append` (FIX.delimiter `cons` coparse (mHeader m))
                `append` coparse (mBody m) 
                `append` coparse (mTrailer m)
            
            ctag = pack . show $ tnum tCheckSum
            ltag = pack . show $ tnum tBodyLength 
            mtag = pack . show $ tnum tMsgType

            header = pack (C.unpack FIX.fixVersion) `snoc` FIX.delimiter
            paddedChecksum m' = pack $ pad 3 $ FIX.checksum m'

packedPad :: (BuilderLike t a) => Int -> Int -> t
packedPad i = pack . pad i

fromFIXMonthYear :: (BuilderLike t a) => CalendarTime -> t
fromFIXMonthYear c = 
    let year = ctYear c; month = 1 + fromEnum (ctMonth c) in
        packedPad 4 year `append` packedPad 2 month

fromFIXUTCData :: (BuilderLike t a) => CalendarTime -> t
fromFIXUTCData c = let day = 1 + ctDay c in
    fromFIXMonthYear c `append` packedPad 2 day 

fromFIXLocalMktDate :: (BuilderLike t a) => CalendarTime -> t
fromFIXLocalMktDate = fromFIXUTCData

fromFIXUTCTimeOnly :: (BuilderLike t a) => CalendarTime -> t
fromFIXUTCTimeOnly c = 
    let min = ctMin c; sec = ctSec c; hours = ctHour c in
        packedPad 2 hours `append` (':' `cons` packedPad 2 min 
        `append` (':' `cons` packedPad 2 sec))

fromFIXUTCTimetamp :: (BuilderLike t a) => CalendarTime -> t
fromFIXUTCTimetamp c =  fromFIXUTCData c `append` 
    ('-' `cons` fromFIXUTCTimeOnly c)


instance Coparser FIXValue where
    coparse (FIXInt a) = decimal a
    coparse (FIXDayOfMonth a) = decimal a
    coparse (FIXFloat a) = realFloat a
    coparse (FIXQuantity a) = realFloat a
    coparse (FIXPrice a) = realFloat a
    coparse (FIXPriceOffset a) = realFloat a
    coparse (FIXAmt a) = realFloat a
    coparse (FIXChar a) = singleton a
    coparse (FIXBool a) 
        | a = singleton 'Y'
        | otherwise = singleton 'N'
    coparse (FIXString a) = pack $ C.unpack a
    coparse (FIXMultipleValueString a) = pack $ C.unpack a
    coparse (FIXCurrency a) = pack $ C.unpack a
    coparse (FIXExchange a) = pack $ C.unpack a
    coparse (FIXUTCTimestamp a) = fromFIXUTCTimetamp a
    coparse (FIXUTCTimeOnly a) = fromFIXUTCTimeOnly a
    coparse (FIXLocalMktDate a) = fromFIXLocalMktDate a
    coparse (FIXUTCDate a) = fromFIXUTCData a
    coparse (FIXMonthYear a) = fromFIXMonthYear a
    coparse (FIXData a) = pack $ C.unpack a
    coparse (FIXDataLen a) = decimal a
    coparse (FIXGroup _ ls) = concat $ map coparse ls


{-instance Coparser (FIXMessage a) where-}
    {-coparse m = coparse (mHeader m) -}
        {-`append` ('\n' `cons` coparse (mBody m) -}
        {-`append` ('\n' `cons` coparse (mTrailer m)))-}

pad :: Int -> Int -> String
pad w i | d <= 0 = t
        | d == 1 = '0' : t
        | otherwise = let prefix = P.replicate d '0' in prefix ++ t
    where
        t = show i
        d = w - P.length t
