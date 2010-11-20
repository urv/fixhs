module Common.FIXCoparser 
	where

import Prelude as P
import Common.FIXMessage 
import Data.ByteString as B
import Data.ByteString.Char8 as C

-- implementation is not efficient 
-- this is just meant for testing 
-- and needs a lot of cleanup
--
-- maybe use some kind of Monoid, resp. Writer, Builder to put together
-- the ByteString. See also blazer-builder - which was started at ZüriHac :-)

externalize :: FIXMessage -> ByteString
externalize (t,v) = tag `append` del `append` val 
	where
		val = externalizeFIXValue v
		tag = C.pack $ show (fromEnum t)
		del = C.singleton '='

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
toString = C.pack . show . fromEnum

checksumTag :: ByteString
checksumTag = toString FIX_CHECKSUM

equals :: ByteString
equals = C.singleton '='
                 
coparse :: [FIXMessage] -> ByteString
-- coparse l = B.concat $ P.map externalize l
coparse l = header `append` checksum' `append` body'
	where 
		-- FIXME: use ByteString directly
		-- checksum' = C.pack $ "9=" ++ show (checksum body') 
		checksum' = checksumTag `append` equals `append` C.pack (show (checksum body'))
		body' = body l
