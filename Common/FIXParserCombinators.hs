module Common.FIXParserCombinators 
	where

import Prelude hiding ( null, tail, head )
import Data.Attoparsec hiding ( takeWhile1 )
import Data.Attoparsec.Char8 
import Data.Char
import Data.ByteString hiding ( pack, putStrLn )
import Data.ByteString.Char8 ( pack, readInt )
import Control.Applicative ( (<$>), (<|>), (*>) )
import System.Time

-- FIXME: explicit imports
import Common.FIXMessage


signed' :: Num a => Parser a -> Parser a
signed' p = (negate <$> (char8 '-' *> p))
       <|> (char8 '+' *> p)
       <|> p

skipFixDelimiter :: Parser ()
skipFixDelimiter = do
    char8 fixDelimiter
    return ()

toFloat :: Parser Float 
toFloat = do 
    a <- signed' decimal
    (m, e) <- (char '.' *> (extract_decimals <$> toString)) <|> return (0, 1)
    skipFixDelimiter
    if a < 0 
        then return $ fromIntegral a - fromIntegral m / fromIntegral e
        else return $ fromIntegral a + fromIntegral m / fromIntegral e
    where
        extract_decimals :: ByteString -> (Int, Int)
        extract_decimals b 
            | null b    = (0, 1)
            | otherwise = 
                let (m', e') = extract_decimals $ tail b
                 in
                (m' * 10 + fromIntegral (head b) - ord '0', 10 * e')

parseIntTill :: Char -> Parser Int
parseIntTill c = do
    i <- signed' decimal
    char8 c
    return i

toInteger' :: ByteString -> Int
toInteger' b = helper 0 b
               where 
                helper i b 
                    | null b    = i
                    | otherwise =   
                        helper (10 * i + fromIntegral (head b) - ord '0') (tail b)
                    
toInt :: Parser Int
toInt = parseIntTill fixDelimiter

toFIXInt :: Parser FIXValue
toFIXInt = FIXInt <$> toInt

toChar :: Parser Char
toChar = do
    c <- anyChar
    skipFixDelimiter
    return c

toString :: Parser ByteString
toString = do 
    str <- takeWhile1 not_fix_delimiter
    skipFixDelimiter
    return str
        where not_fix_delimiter w = w /= fixDelimiter

toString_ :: Parser ByteString
toString_ = takeWhile1 not_fix_delimiter
        where not_fix_delimiter w = w /= fixDelimiter

toFIXString :: Parser FIXValue
toFIXString = FIXString <$> toString

toTag :: Parser Int
toTag = parseIntTill '='
    
toBool :: Parser Bool
toBool = do
    c <- (char 'Y' <|> char 'N')
    skipFixDelimiter
    case c of
        'Y' -> return True
        'N' -> return False

toFIXBool :: Parser FIXValue
toFIXBool = FIXBool <$> toBool

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

toUTCTimestamp :: Parser CalendarTime
toUTCTimestamp = do
   i <- parseIntTill '-'
   let year  = i `div` 10000
   let rest  = i `mod` 10000
   let month = rest `div` 100
   let day   = rest `mod` 100
   hours   <- parseIntTill ':'
   minutes <- parseIntTill ':'
   (sec, millis) <- toSecMillis
   return CalendarTime {
       ctYear  = year
     , ctMonth = toEnum $ month - 1
     , ctDay   = day
     , ctHour  = hours
     , ctMin   = minutes
     , ctSec   = sec
     , ctPicosec = 0
     , ctWDay  = Monday
     , ctYDay  = 0
     , ctTZName = "UTC"
     , ctTZ    = 0
     , ctIsDST = True
   }

toUTCTimeOnly :: Parser CalendarTime
toUTCTimeOnly = do
   hours   <- parseIntTill ':'
   minutes <- parseIntTill ':'
   (sec, millis) <- toSecMillis
   return CalendarTime {
       ctYear  = 0
     , ctMonth = toEnum 0
     , ctDay   = 0
     , ctHour  = hours
     , ctMin   = minutes
     , ctSec   = sec
     , ctPicosec = 0
     , ctWDay  = Monday
     , ctYDay  = 0
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
     , ctWDay  = Monday
     , ctYDay  = 0
     , ctTZName = "UTC"
     , ctTZ    = 0
     , ctIsDST = True
   }

toUTCDate :: Parser CalendarTime
toUTCDate = toLocalMktDate

toTime :: Parser CalendarTime
toTime = toUTCTimestamp 
          <|> toUTCTimeOnly 
          <|> toUTCDate 
          <|> toLocalMktDate
