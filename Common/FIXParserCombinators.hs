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

data FIXValue = FIXInt Int | FIXBool Bool | FIXString ByteString

fix_delimiter :: Char
-- fix_delimiter = '\0'
fix_delimiter = '|'

signed' :: Num a => Parser a -> Parser a
signed' p = (negate <$> (char8 '-' *> p))
       <|> (char8 '+' *> p)
       <|> p

skip_fix_delimiter :: Parser ()
skip_fix_delimiter = do
    char8 fix_delimiter
    return ()

to_float :: Parser Float 
to_float = do 
    a <- signed' decimal
    (m, e) <- (char '.' *> (extract_decimals <$> to_string)) <|> return (0, 1)
    skip_fix_delimiter
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

parse_int_till :: Char -> Parser Int
parse_int_till c = do
    i <- signed' decimal
    char8 c
    return i

parse_int_till' :: Char -> Parser FIXValue
parse_int_till' c = do
    i <- signed' decimal
    char8 c
    return $ FIXInt i

toInteger' :: ByteString -> Int
toInteger' b = helper 0 b
               where 
                helper i b 
                    | null b    = i
                    | otherwise =   
                        helper (10 * i + fromIntegral (head b) - ord '0') (tail b)
                    
to_int :: Parser Int
to_int = parse_int_till fix_delimiter

toFIXInt :: Parser FIXValue
toFIXInt = parse_int_till' fix_delimiter

to_char :: Parser Char
to_char = do
    c <- anyChar
    skip_fix_delimiter
    return c

to_string :: Parser ByteString
to_string = do 
    str <- takeWhile1 not_fix_delimiter
    skip_fix_delimiter
    return str
        where not_fix_delimiter w = w /= fix_delimiter

toFIXString :: Parser FIXValue
toFIXString = do 
    str <- takeWhile1 not_fix_delimiter
    skip_fix_delimiter
    return $ FIXString str
        where not_fix_delimiter w = w /= fix_delimiter

to_tag :: Parser Int
to_tag = parse_int_till '='
    
to_bool :: Parser FIXValue
to_bool = do
    c <- (char 'Y' <|> char 'N')
    skip_fix_delimiter
    case c of
        'Y' -> return $ FIXBool True
        'N' -> return $ FIXBool False

to_sec_millis :: Parser (Int, Int)
to_sec_millis = do
   (sec, mil) <- (to_int >>= only_seconds) <|> read_sec_millis
   return (sec, mil)
   where
        only_seconds :: Int -> Parser (Int, Int)
        only_seconds sec = return (sec, 0)

        read_sec_millis :: Parser (Int, Int)
        read_sec_millis = do
            sec' <- parse_int_till '.'
            mil' <- to_int
            return (sec', mil')

to_UTCTimestamp :: Parser CalendarTime
to_UTCTimestamp = do
   i <- parse_int_till '-'
   let year  = i `div` 10000
   let rest  = i `mod` 10000
   let month = rest `div` 100
   let day   = rest `mod` 100
   hours   <- parse_int_till ':'
   minutes <- parse_int_till ':'
   (sec, millis) <- to_sec_millis
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

to_UTCTimeOnly :: Parser CalendarTime
to_UTCTimeOnly = do
   hours   <- parse_int_till ':'
   minutes <- parse_int_till ':'
   (sec, millis) <- to_sec_millis
   return CalendarTime {
       ctYear  = 0
     , ctMonth = toEnum $ 0
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

to_LocalMktDate :: Parser CalendarTime
to_LocalMktDate = do
   i <- parse_int_till '-'
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

to_UTCDate :: Parser CalendarTime
to_UTCDate = to_LocalMktDate

to_time :: Parser CalendarTime
to_time = to_UTCTimestamp 
          <|> to_UTCTimeOnly 
          <|> to_UTCDate 
          <|> to_LocalMktDate
