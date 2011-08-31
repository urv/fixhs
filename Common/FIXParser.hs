module Common.FIXParser 
	( messageP
    , groupP
    , _nextP
    , _nextP'
    , nm
    , toFIXInt
    , toFIXChar
    , toFIXString
    , toFIXDayOfMonth
    , toFIXFloat
    , toFIXQuantity
    , toFIXPrice
    , toFIXPriceOffset
    , toFIXAmt
    , toFIXBool
    , toFIXMultipleValueString
    , toFIXCurrency
    , toFIXExchange
    , toFIXUTCTimestamp
    , toFIXUTCTimeOnly
    , toFIXLocalMktDate
    , toFIXData
    , toFIXDataLen
	) where

import Prelude hiding ( take, null, head, tail )
import Common.FIXMessage 
    ( FIXTags, FIXSpec (..), FIXMessage (..)
    , FIXTag (..), FIXValue (..), FIXMessageSpec (..)
    , FIXValues, FIXGroupSpec (..) )
import qualified Common.FIXMessage as FIX ( delimiter, checksum )
import Common.FIXParserCombinators 
import Data.Attoparsec ( count, many, string, take, parseOnly )
import Data.Char ( ord )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C ( pack ) 
import Data.Maybe ( fromMaybe )
import qualified Data.LookupTable as LT ( lookup, fromList, insert )
import Control.Applicative ( (<$>) )
import Control.Monad ( liftM )

-- Lookup the parser for a given FIX tag.
parseFIXTag :: FIXTags -> Parser (Int, FIXValue)
parseFIXTag tags = do 
    l <- toTag
    let tag' = LT.lookup l tags
        parser' = fromMaybe (fail $ "unknown tag " ++ show l) $ 
                    liftM tparser tag' 
        in fmap ((,) l) parser' 


-- parse a specific tag and its value
tagP :: FIXTag -> Parser FIXValue
tagP tag = do l <- toTag -- read out tag in message
              if l == tnum tag then -- if the two tags coincide read the value
                tparser tag else fail ""

-- parse all the specificed tags and their corresponding values
tagsP :: FIXTags -> Parser FIXValues
tagsP ts = let tag' = parseFIXTag ts in 
               liftM LT.fromList $ many tag'

-- parse a value of type FIX group
groupP :: FIXGroupSpec -> Parser FIXValue
groupP spec = let numTag = gsLength spec in 
                 do FIXInt n <- tagP numTag -- number of submessages
                    b <- count n submsg     -- parse the submessages
                    return $ FIXGroup n b
              where
                  submsg :: Parser FIXValues
                  submsg = 
                    let sepTag = gsSeperator spec
                        insertSep = LT.insert (tnum sepTag)
                    in do h <- tagP sepTag -- The head of the message
                          insertSep h <$> tagsP (gsBody spec)

-- Parse a FIX message. The parser fails when the checksum 
-- validation fails.
_nextP :: Parser ByteString
_nextP = do 
    (hchksum, len) <- _header'
    msg <- take len 
    c <- _calcChksum'
    let chksum = (hchksum + FIX.checksum msg) `mod` 256  in
        if chksum == c then return msg else fail "checksum not valid"

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

-- Parse a FIX message. There is no validation of the FIX message done.
_nextP' :: Parser ByteString
_nextP' = do 
          msg <- take =<< _numBytes
          toTag >> toInt
          return msg 
    where
        _numBytes = 
            let 
                skipHeader = 
                    string (C.pack "8=") >> toString >> 
                    string (C.pack "9=") 
            in 
                skipHeader >> toInt

 -- Parse a FIX message out of the stream
messageP :: FIXSpec -> Parser FIXMessage
messageP spec = 
    let headerP  = tagsP $ fsHeader spec  -- parse header
        trailerP = tagsP $ fsTrailer spec -- parse trailer
        bodyP mtype =                     -- parse body
            let allSpecs = fsMessages spec
                msgSpec = fromMaybe (error "no message") $
                                LT.lookup mtype allSpecs
                msgBodyTags = msBody msgSpec
            in 
                tagsP msgBodyTags
    in do
        h <- headerP
        let mt' = fromMaybe (error "next tag should be 35") (LT.lookup 35 h) 
            mt = case mt' of 
                      FIXString t -> t 
                      _ -> undefined
        b <- bodyP mt
        t <- trailerP 
        return FIXMessage { mHeader = h, mBody = b, mTrailer = t }

nm :: FIXSpec -> Parser FIXMessage
nm spec = do msg <- _nextP
             case parseOnly (messageP spec) msg of
                 Right ts -> return ts
                 Left err -> fail err 

-- FIX value parsers 
toFIXInt = FIXInt <$> toInt
toFIXDayOfMonth = FIXDayOfMonth <$> toInt
toFIXFloat = FIXFloat <$> toFloat
toFIXQuantity = FIXQuantity <$> toFloat
toFIXPrice = FIXPrice <$> toFloat
toFIXPriceOffset = FIXPriceOffset <$> toFloat
toFIXAmt = FIXAmt <$> toFloat
toFIXBool = FIXBool <$> toBool
toFIXString = FIXString <$> toString
toFIXMultipleValueString = FIXMultipleValueString <$> toString
toFIXCurrency = FIXCurrency <$> toString
toFIXExchange = FIXExchange <$> toString
toFIXData = FIXData <$> toString
toFIXDataLen = FIXDataLen <$> toInt
toFIXUTCTimestamp = FIXUTCTimestamp <$> toUTCTimestamp
toFIXUTCTimeOnly = FIXUTCTimestamp <$> toUTCTimeOnly
toFIXLocalMktDate = FIXLocalMktDate <$> toLocalMktDate
toFIXChar = FIXChar <$> toChar
