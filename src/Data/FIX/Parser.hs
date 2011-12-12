-- Module  : Data.FIX.Parser
-- License : LGPL-2.1 

{-# OPTIONS_GHC  -fno-warn-missing-signatures #-}

module Data.FIX.Parser 
    ( 
-- * Introduction
-- | In order to get a Parser 'FIXMessage' 'FIXSpec' you can 
--
-- @
--import qualified Data.FIX.Parser as FIX ( nextP, messageP )
--
--FIX.nextP >>= FIX.messageP
-- @
      messageP
    , groupP
    , nextP
    , nextP'
    , toFIXInt
    , toFIXChar
    , toFIXString
    , toFIXDouble
    , toFIXBool
    , toFIXMultipleValueString
    , toFIXTimestamp
    , toFIXTimeOnly
    , toFIXData
    , toFIXDateOnly
    , toFIXMonthYear
    , tBeginString
    , tCheckSum
    , tBodyLength
    , tMsgType
    ) where

import Prelude hiding ( take, null, head, tail )
import Data.FIX.Message 
    ( FIXGroupElement(..), FIXTags, FIXSpec (..), FIXMessage (..)
    , FIXTag (..), FIXValue (..), FIXMessageSpec (..)
    , FIXValues, FIXGroupSpec (..) )
import qualified Data.FIX.Message as FIX ( checksum )
import qualified Data.FIX.Common as FIX ( delimiter )
import Data.FIX.ParserCombinators ( toTag, toString, toInt, toInt', toBool, toChar, toTimestamp, toDateOnly, toMonthYear, toTimeOnly, toDouble )
import Data.Attoparsec ( parseOnly, option, Result(..), Parser, count, string, take )
import Data.Char ( ord )
import Data.ByteString ( ByteString )
import Data.FIX.Arbitrary ()
import qualified Data.ByteString.Char8 as C ( length, pack ) 
import Data.Maybe ( fromMaybe )
import qualified Data.LookupTable as LT ( new, toList, lookup, fromList, insert )
import Control.Applicative ( (<$>) )
import Control.Monad ( liftM )
import Test.QuickCheck ( arbitrary )

-- parse a specific tag and its value
tagP :: FIXTag -> Parser FIXValue
tagP tag = do l <- toTag -- read out tag in message
              if l == tnum tag then -- if the two tags coincide read the value
                tparser tag else fail "wrong tag"

-- parse all the specificed tags and their corresponding values
tagsP :: FIXTags -> Parser FIXValues
tagsP ts = option LT.new (insertValue LT.new)
	where
           insertValue :: FIXValues -> Parser FIXValues
	   insertValue t = do 
	   	l <- nextTag
		val <- tparser l
		let t' = LT.insert (tnum l) val t 
		option t' (insertValue t') 
	   
	   nextTag :: Parser FIXTag
	   nextTag = do 
	   	tag' <- toTag 
		let mtag' = liftM return $ LT.lookup tag' ts 
		fromMaybe (fail "") mtag'

	   {-errorMsg l = error $ "unknown tag " ++ show l ++-}
		   {-"\nknown tags are " ++ show (LT.toList ts)-}

-- parse a value of type FIX group
groupP :: FIXGroupSpec -> Parser FIXValue
groupP spec = let numTag = gsLength spec in 
		 do n <- toInt           -- number of submessages
                    b <- count n submsg  -- parse the submessages
                    return $ FIXGroup n b
              where
                  submsg :: Parser FIXGroupElement
                  submsg = 
                    let sepTag = gsSeperator spec
                    in do s  <- tagP sepTag -- The seperator of the message
		    	  vs <- tagsP (gsBody spec) -- The rest of the message
			  return (FIXGroupElement (tnum sepTag) s vs)

-- | Match the next FIX message (only text) in the stream. The checksum is
-- validated.
nextP :: Parser ByteString
nextP = do 
    (hchksum, len) <- _header'
    msg <- take len 
    let chksum = (hchksum + FIX.checksum msg) `mod` 256  
    c <- _calcChksum'
    if chksum == c then return msg 
        else fail $ "checksum is not valid: is " ++ 
                    show chksum  ++ " should be " ++ show c
    where
        -- Parse header and return checksum and length.
        -- A header always starts with the version tag (8)  
        -- followed by the length tag (9). Note: these 2 tags
        -- are included in the checksum
        _header' :: Parser (Int, Int)
        _header' = do 
            c1 <- (FIX.checksum <$> string (C.pack "8="))
            c2 <- (FIX.checksum <$> toString)
            c3 <- (FIX.checksum <$> string (C.pack "9="))
            l <- toString
            let c4 = FIX.checksum l
                c = (c1 + c2 + c3 + c4 + 2 * ord FIX.delimiter) `mod` 256
            return (c, toInt' l)

        _calcChksum' :: Parser Int
        _calcChksum' = string (C.pack "10=") >> toInt

-- | Match the next FIX message (only text) in the stream. The checksum is NOT
-- validated.
nextP' :: Parser ByteString
nextP' = do 
          msg <- take =<< _numBytes
          _ <- toTag >> toInt
          return msg 
    where
        _numBytes = 
            let 
                skipHeader = 
                    string (C.pack "8=") >> toString >> 
                    string (C.pack "9=") 
            in 
                skipHeader >> toInt

-- | Given the FIX specification deserialize the FIX message.
messageP :: FIXSpec -> ByteString -> Parser (FIXMessage FIXSpec)
messageP spec msg = 
    let headerP'  = tagsP $ fsHeader spec  -- parse header
        trailerP' = tagsP $ fsTrailer spec -- parse trailer
        bodyP' mtype =                     -- parse body
            let allSpecs = fsMessages spec
                msgSpec = fromMaybe (error "no message") $
                                LT.lookup mtype allSpecs
                msgBodyTags = msBody msgSpec
            in 
                tagsP msgBodyTags

        fixP' = do
                FIXString mt <- tagP tMsgType
                h <- headerP'
                b <- bodyP' mt
                t <- trailerP' 
                return FIXMessage 
                    { mContext = spec
                    , mType = mt
                    , mHeader = h
                    , mBody = b
                    , mTrailer = t }
    in 
        case parseOnly fixP' msg of
             Left err  -> fail err 
	     Right msg -> return msg


-- FIX value parsers 
toFIXInt = FIXInt <$> toInt
toFIXDouble = FIXDouble <$> toDouble
toFIXBool = FIXBool <$> toBool
toFIXString = FIXString <$> toString
toFIXMultipleValueString = FIXMultipleValueString <$> toString
toFIXData = FIXData <$> toString
toFIXTimestamp = FIXTimestamp <$> toTimestamp
toFIXTimeOnly = FIXTimeOnly <$> toTimeOnly
toFIXChar = FIXChar <$> toChar
toFIXDateOnly = FIXDateOnly <$> toDateOnly
toFIXMonthYear = FIXMonthYear <$> toMonthYear


tBeginString :: FIXTag
tBeginString = FIXTag
    { tName = "BeginString"
    , tnum = 8
    , tparser = toFIXString
    , arbitraryValue = FIXString <$> arbitrary }


tBodyLength :: FIXTag
tBodyLength = FIXTag
    { tName = "BodyLength"
    , tnum = 9
    , tparser = toFIXInt 
    , arbitraryValue = FIXInt <$> arbitrary }

tMsgType :: FIXTag
tMsgType = FIXTag
    { tName = "MsgType"
    , tnum = 35
    , tparser = toFIXString 
    , arbitraryValue = FIXString <$> arbitrary }

tCheckSum :: FIXTag
tCheckSum = FIXTag
    { tName = "CheckSum"
    , tnum = 10
    , tparser = toFIXInt 
    , arbitraryValue = FIXInt <$> arbitrary }
