{-# LANGUAGE FlexibleInstances #-}

module Common.FIXCoparser 
	(
	-- 
	coparse
	) where

import Prelude as P
import Common.FIXMessage ( FIXSpec, FIXMessage, tnum, FIXValue (..) )
import Common.FIXParser ( tBeginString, tBodyLength, tCheckSum, tMsgType )
import qualified Common.FIXMessage as FIX ( checksum, delimiter )
import qualified Data.FIX.Common as FIX ( fixVersion )
import Data.Coparser ( Coparser (..) )
import Data.ByteString as B
import Data.ByteString.Char8 as C ( unpack, length, cons, pack, singleton, append, snoc )
import qualified Data.LookupTable as LT
import Common.FIXMessage ( FIXValues, FIXValue (..), FIXMessage (..) )

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


-- FIX body
externalize :: (Int, FIXValue) -> ByteString
externalize (t,v) = tag `append` del `append` val 
	where
		val = externalize' v
		tag = C.pack $ show t
		del = C.singleton '='

externalize' :: FIXValue -> ByteString
externalize' (FIXInt i) = C.pack $ show i 
externalize' (FIXBool b) = C.pack $ show b  
externalize' (FIXString s) = s
externalize' _ = undefined


coValues :: FIXValues -> String
coValues vs = let values = LT.toList vs in
    P.concatMap mapV values
        where
            mapV (k, FIXGroup l vs') = 
                show k ++ "=" ++ show l ++ 
                    FIX.delimiter : P.concatMap coValues vs'
            mapV (k, v) = show k ++ "=" ++ show v ++ FIX.delimiter : ""


body :: FIXMessage a -> ByteString
-- body l = B.concat $ P.map ((C.cons FIX.delimiter) . externalize) l
-- body l = B.intercalate (C.pack FIX.delimiter) (externalize l)
{-body l = let ts = LT.toList l in -}
    {-B.intercalate (C.singleton FIX.delimiter) (P.map externalize ts)-}
body m = C.pack $ (coValues $ mHeader m) 
            ++ (coValues $ mBody m) 
            ++ (coValues $ mTrailer m) 

{-toString :: FIXTag -> ByteString-}
{-toString = C.pack . show -}

                 
-- externalize the FIXMessage
instance Coparser (FIXMessage FIXSpec) where
    coparse l = msg' `append` chksum' `C.snoc` FIX.delimiter
        where 
            msg' = header `append` len' `C.snoc` FIX.delimiter `append`  body'
            chksum' = checksumTag `append` equals `append` paddedChecksum msg'
            len' = lengthTag `append` equals `append` C.pack (show $ C.length body')
            mtype' = msgTypeTag `append` equals `append` mType l
            body' = mtype' `C.snoc` FIX.delimiter `append` body l 
            
            checksumTag = C.pack . show $ tnum tCheckSum
            lengthTag = C.pack . show $ tnum tBodyLength 
            msgTypeTag = C.pack . show $ tnum tMsgType

            equals = C.singleton '='
            header = C.snoc FIX.fixVersion FIX.delimiter -- FIX Header

            paddedChecksum :: ByteString -> ByteString
            paddedChecksum = checksum' . FIX.checksum
                where
                    checksum' :: Int -> ByteString
                    checksum' b | b < 10 = C.pack "00" `append` num
                                | b < 100 = C.cons '0' num
                                | otherwise = num
                                where num = C.pack (show b)

fromFIXUTCTimetamp = const "0000000-00:00:00"
fromFIXUTCTimeOnly = const "00:00:00"
fromFIXLocalMktDate = const "00000000"
fromFIXUTCData = const "00000000"
fromFIXMonthYear  = const "000000"



instance Show FIXValue where
    show (FIXInt a) = show a
    show (FIXDayOfMonth a) = show a
    show (FIXFloat a) = "0"
    show (FIXQuantity a) = "0"
    show (FIXPrice a) = "0"
    show (FIXPriceOffset a) = "0"
    show (FIXAmt a) = "0"
    show (FIXChar a) = a : ""
    show (FIXBool a) 
        | True = "Y"
        | False = "N"
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
    show (FIXGroup _ _) = "group"


instance Show (FIXMessage a) where
    show m = show (mHeader m) ++ "\n"
        ++ show (mBody m) ++ "\n" ++ show (mTrailer m) 
