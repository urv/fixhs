module Common.FIXParserCombinators 
    ( toTag
    , toString
    , toInt'
    , toInt
    , toFloat
    , toBool
    , toUTCTimestamp
    , toUTCTimeOnly
    , toLocalMktDate
    , toChar
    , toUTCDate
    , toTime
    , toMonthYear

    -- exporting Attoparsec
    , Data.Attoparsec.Parser
    , Data.Attoparsec.Zepto.parse
	) where

import Prelude hiding ( null, tail, head )
import Data.Attoparsec ( Parser )
import qualified Data.Attoparsec.Zepto ( parse )
import Data.Attoparsec.Char8 ( char, char8, anyChar, takeWhile1, decimal )
import Data.Char ( ord )
import Data.ByteString hiding ( pack, putStrLn )
import Control.Applicative ( (<$>), (<|>), (*>) )
import System.Time ( CalendarTime (..) )
import qualified Common.FIXMessage as FIX ( delimiter )


signed' :: Num a => Parser a -> Parser a
signed' p = (negate <$> (char8 '-' *> p))
       <|> (char8 '+' *> p)
       <|> p

skipFIXDelimiter :: Parser ()
skipFIXDelimiter = char8 FIX.delimiter >> return ()

toFloat :: Parser Float 
toFloat = do 
    a <- signed' decimal :: Parser Integer
    (m, e) <- (char '.' *> (extract_decimals <$> toString)) <|> return (0, 1)
    skipFIXDelimiter
    if a < 0 
        then return $ fromIntegral a - fromIntegral m / fromIntegral e
        else return $ fromIntegral a + fromIntegral m / fromIntegral e
    where
        extract_decimals :: ByteString -> (Int, Int)
        extract_decimals b 
            | null b    = (0, 1)
            | otherwise = 
                let (m', e') = extract_decimals $ tail b in
                   (m' * 10 + fromIntegral (head b) - ord '0', 10 * e')

parseIntTill :: Char -> Parser Int
parseIntTill c = do
    i <- signed' decimal
    _ <- char8 c
    return i

toInt' :: ByteString -> Int
toInt' b = helper 0 b
           where 
                helper i j 
                    | null j    = i
                    | otherwise =   
                        helper (10 * i + fromIntegral (head j) - ord '0') (tail j)
                    
toInt :: Parser Int
toInt = parseIntTill FIX.delimiter


toChar :: Parser Char
toChar = do
    c <- anyChar
    skipFIXDelimiter
    return c

toString :: Parser ByteString
toString = do 
    str <- takeWhile1 (/= FIX.delimiter)
    skipFIXDelimiter
    return str


toTag :: Parser Int
toTag = parseIntTill '='
    
toBool :: Parser Bool
toBool = do
    c <- (char 'Y' <|> char 'N')
    skipFIXDelimiter
    case c of
        'Y' -> return True
        'N' -> return False
        _ -> error "wrong boolean FIX value"

toSecMillis :: Parser (Int, Int)
toSecMillis = do
   (sec, mil) <- (toInt >>= only_seconds) <|> read_sec_millis
   return (sec, mil)
   where
        only_seconds :: Int -> Parser (Int, Int)
        only_seconds sec = return (sec, 0)

        read_sec_millis :: Parser (Int, Int)
        read_sec_millis = do
            sec' <- parseIntTill '.'
            mil' <- toInt
            return (sec', mil')

-- one milli seconds is 10^9 picoseconds 
picosPerMilli :: Int
picosPerMilli = 1000000000

toUTCTimestamp :: Parser CalendarTime
toUTCTimestamp = do
   i <- parseIntTill '-'
   let year  = i `div` 10000
   let rest  = i `mod` 10000
   let month = rest `div` 100
   let day   = rest `mod` 100
   hours   <- parseIntTill ':'
   minutes <- parseIntTill ':'
   (sec, milli) <- toSecMillis
   return CalendarTime {
       ctYear  = year
     , ctMonth = toEnum $ month - 1
     , ctDay   = day
     , ctHour  = hours
     , ctMin   = minutes
     , ctSec   = sec
     , ctPicosec = toInteger $ milli * picosPerMilli
     , ctWDay  = undefined 
     , ctYDay  = 0
     , ctTZName = "UTC"
     , ctTZ    = 0
     , ctIsDST = True
   }

toUTCTimeOnly :: Parser CalendarTime
toUTCTimeOnly = do
   hours   <- parseIntTill ':'
   minutes <- parseIntTill ':'
   (sec, milli) <- toSecMillis
   return CalendarTime {
       ctYear  = 0
     , ctMonth = toEnum 0
     , ctDay   = 0
     , ctHour  = hours
     , ctMin   = minutes
     , ctSec   = sec
     , ctPicosec = toInteger $ milli * picosPerMilli
     , ctWDay  = undefined
     , ctYDay  = undefined
     , ctTZName = "UTC"
     , ctTZ    = 0
     , ctIsDST = True
   }

toLocalMktDate :: Parser CalendarTime
toLocalMktDate = do
   i <- parseIntTill '-'
   let year  = i `div` 10000
   let rest  = i `mod` 10000
   let month = rest `div` 100
   let day   = rest `mod` 100
   return CalendarTime {
       ctYear  = year
     , ctMonth = toEnum $ month - 1
     , ctDay   = day
     , ctHour  = 0
     , ctMin   = 0 
     , ctSec   = 0
     , ctPicosec = 0
     , ctWDay  = undefined
     , ctYDay  = undefined
     , ctTZName = "UTC"
     , ctTZ    = 0
     , ctIsDST = True
   }

toUTCDate :: Parser CalendarTime
toUTCDate = toLocalMktDate

toMonthYear :: Parser CalendarTime
toMonthYear = do
   i <- toInt
   let year  = i `div` 10000
   let rest  = i `mod` 10000
   let month = rest `div` 100
   return CalendarTime {
       ctYear  = year
     , ctMonth = toEnum $ month - 1
     , ctDay   = 0
     , ctHour  = 0
     , ctMin   = 0 
     , ctSec   = 0
     , ctPicosec = 0
     , ctWDay  = undefined
     , ctYDay  = undefined
     , ctTZName = "UTC"
     , ctTZ    = 0
     , ctIsDST = True
   }

toTime :: Parser CalendarTime
toTime = toUTCTimestamp 
          <|> toUTCTimeOnly 
          <|> toUTCDate 
          <|> toLocalMktDate
