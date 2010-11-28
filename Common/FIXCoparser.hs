module Common.FIXCoparser 
	(
	-- 
	coparse
	) where

import Prelude as P
import Common.FIXMessage 
import Data.ByteString as B
import Data.ByteString.Char8 as C

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

-- FIX header
header :: ByteString
-- header = C.pack "8=FIX.4.2\SOH"
header = C.snoc fix_version fix_delimiter

checksumTag :: ByteString
checksumTag = toString FIX_CHECKSUM

lengthTag :: ByteString
lengthTag = toString FIX_MSG_LENGTH

-- FIX body
externalize :: FIXMessage -> ByteString
externalize (t,v) = tag `append` del `append` val 
	where
		val = externalize' v
		tag = C.pack $ show (fromEnum t)
		del = C.singleton '='

externalize' :: FIXValue -> ByteString
externalize' (FIXInt i) = C.pack $ show i 
externalize' (FIXBool b) = C.pack $ show b  
externalize' (FIXString s) = s

body :: [FIXMessage] -> ByteString
-- body l = B.concat $ P.map ((C.cons fix_delimiter) . externalize) l
-- body l = B.intercalate (C.pack fix_delimiter) (externalize l)
body l = B.intercalate (C.singleton fix_delimiter) (P.map externalize l)

toString :: FIXTag -> ByteString
toString = C.pack . show . fromEnum

equals :: ByteString
equals = C.singleton '='
                 
-- externalize the FIXMessage
coparse :: [FIXMessage] -> ByteString
coparse l = message' `append` checksum'
	where 
		message' = header `append` length' `C.snoc` fix_delimiter `append` body' `C.snoc` fix_delimiter
		checksum' = checksumTag `append` equals `append` C.pack (show (checksum body''))
		length' = lengthTag `append` equals `append` C.pack (show (flength body'))
		body'' = C.filter (not . (==fix_delimiter)) body'
		body' = body l
