-- Module  : Data.FIX.Coparser
-- License : LGPL-2.1 

{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances #-}

module Data.FIX.Coparser ( coparse) where

import Prelude as P hiding (concat)
import Data.FIX.Message 
    ( FIXGroupElement(..), FIXSpec (..), FIXMessage (..)
    , tnum, FIXValues, FIXValue (..) )
import Data.FIX.Parser ( tBeginString, tBodyLength, tCheckSum, tMsgType )
import qualified Data.FIX.Message as FIX ( checksum, delimiter )
import Data.Coparser ( Coparser (..), BuilderLike, 
    pack, concat, append, cons, snoc, singleton, decimal, realFloat )
import qualified Data.Coparser as Text ( length )
import Data.ByteString ( ByteString )
import Data.ByteString.Char8 as C ( unpack )
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
            _serialize = concatMap _serValue

            _serValue (k, FIXGroup i ls) = 
                let sub = concat $ map coparse ls
                    delim = FIX.delimiter         
                in
                    decimal k `append` ('=' `cons` decimal i `append` (delim `cons` sub))

            _serValue (k, v) = 
                let delim = FIX.delimiter in
                    decimal k `append` ('=' `cons` coparse v `append` singleton delim)



-- externalize the FIXMessage
instance Coparser (FIXMessage FIXSpec) where
    coparse m = msg' `append` chksum' 
        where 
            msg' = header `append` (len' `append` (FIX.delimiter `cons` body'))

            chksum' = ctag `append` ('=' `cons` (paddedChecksum msg' `snoc` FIX.delimiter))
            len' = ltag `append` ('=' `cons` decimal (Text.length body'))
            mtype' = mtag `append` ('=' `cons` pack (C.unpack $ mType m))
            body' = mtype' 
                `append` (FIX.delimiter `cons` coparse (mHeader m))
                `append` coparse (mBody m) 
                `append` coparse (mTrailer m)
            
            btag = decimal $ tnum tBeginString
            ctag = decimal $ tnum tCheckSum
            ltag = decimal $ tnum tBodyLength 
            mtag = decimal $ tnum tMsgType

            version = fsVersion $ mContext m
            header = btag `append` ('=' `cons` pack version `snoc` FIX.delimiter)
            paddedChecksum m' = FIX.checksum m' `pad` 3 


fromFIXMonthYear :: (BuilderLike t a) => CalendarTime -> t
fromFIXMonthYear c = 
    let year = ctYear c; month = 1 + fromEnum (ctMonth c) in
        (year `pad` 4) `append` (month `pad` 2)

fromFIXDateOnly :: (BuilderLike t a) => CalendarTime -> t
fromFIXDateOnly c = let day = ctDay c in
    fromFIXMonthYear c `append` (day `pad` 2)

fromFIXTimeOnly :: (BuilderLike t a) => CalendarTime -> t
fromFIXTimeOnly c = let m = ctMin c; s = ctSec c; h = ctHour c in
        (h `pad` 2) `append` (':' `cons` (m `pad` 2) 
        `append` (':' `cons` (s `pad` 2 )))

fromFIXTimetamp :: (BuilderLike t a) => CalendarTime -> t
fromFIXTimetamp c = fromFIXDateOnly c `append` 
    ('-' `cons` fromFIXTimeOnly c)

instance Coparser FIXGroupElement where
    coparse (FIXGroupElement t s vs) = 
   	 let delim = FIX.delimiter in
	     decimal t `append` ('=' `cons` coparse s)
	     `append` (delim `cons` coparse vs) 

instance Coparser FIXValue where
    coparse (FIXInt a) = decimal a
    coparse (FIXDouble a) = realFloat a
    coparse (FIXChar a) = singleton a
    coparse (FIXBool a) 
        | a = singleton 'Y'
        | otherwise = singleton 'N'
    coparse (FIXString a) = pack $ C.unpack a
    coparse (FIXMultipleValueString a) = pack $ C.unpack a
    coparse (FIXTimestamp a) = fromFIXTimetamp a
    coparse (FIXTimeOnly a) = fromFIXTimeOnly a
    coparse (FIXDateOnly a) = fromFIXDateOnly a
    coparse (FIXMonthYear a) = fromFIXMonthYear a
    coparse (FIXData a) = pack $ C.unpack a
    coparse (FIXGroup n ls) = 
    	decimal n `snoc` FIX.delimiter 
	`append` concat (map coparse ls)


{-instance Coparser (FIXMessage a) where-}
    {-coparse m = coparse (mHeader m) -}
        {-`append` ('\n' `cons` coparse (mBody m) -}
        {-`append` ('\n' `cons` coparse (mTrailer m)))-}

pad :: (BuilderLike a c) => Int -> Int -> a
pad i w | d <= 0 = decimal i
        | d == 1 = '0' `cons` decimal i
        | otherwise = let prefix = P.replicate d '0' in 
            pack prefix `append` decimal i
    where
        d = w - len' i

        len' i' = if i' < 10 then 1 else 1 + len' (i' `div` 10)
