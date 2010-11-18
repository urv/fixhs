module Common.FIXCoparser 
	where

import Prelude as P
import Common.FIXMessage 
import Data.ByteString as B
import Data.ByteString.Char8 as C

-- implementation is not efficient 
-- this is just meant for testing 
-- and needs a lot of cleanup
externalize :: FIXMessage -> ByteString
externalize (t,v) = tag `append` del `append` val 
	where
		val = externalizeFIXValue v
		tag = C.pack $ show (fromEnum t)
		del = C.pack "="

externalizeFIXValue :: FIXValue -> ByteString
externalizeFIXValue (FIXInt i) = C.pack $ show i 
externalizeFIXValue (FIXBool b) = C.pack $ show b  
externalizeFIXValue (FIXString s) = s

body :: [FIXMessage] -> ByteString
-- body l = intercalate (C.pack fix_delimiter) (P.map (cons fix_delimiter . externalize) l)
body l = B.concat $ P.map ((C.cons fix_delimiter) . externalize) l

header :: ByteString
header = C.pack "8=FIX.4.1|"

toString :: FIXTag -> ByteString
toString = B.pack . show . fromEnum
                 
coparse :: [FIXMessage] -> ByteString
-- coparse l = B.concat $ P.map externalize l
coparse l = header `append` checksum' `append` body'
	where 
		-- FIXME: use ByteString directly
		-- checksum' = C.pack $ "9=" ++ show (checksum body') 
		checksum' = C.pack $ toString FIX_CHECKSUM ++ "=" ++ show (checksum body')
		body' = body l
