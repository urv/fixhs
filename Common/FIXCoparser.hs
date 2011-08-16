module Common.FIXCoparser 
	(
	-- 
	coparse
	) where

import Prelude as P
import Common.FIXMessage hiding ( checksum' )
import Data.ByteString as B
import Data.ByteString.Char8 as C
import qualified Data.LookupTable as LT

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
header = C.snoc fixVersion fixDelimiter

checksumTag :: ByteString
checksumTag = undefined -- toString FIX_CHECKSUM

lengthTag :: ByteString
lengthTag = undefined -- toString FIX_MSG_LENGTH

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

body :: FIXMessage -> ByteString
-- body l = B.concat $ P.map ((C.cons fixDelimiter) . externalize) l
-- body l = B.intercalate (C.pack fixDelimiter) (externalize l)
body (Tokens l) = let ts = LT.toList l in 
    B.intercalate (C.singleton fixDelimiter) (P.map externalize ts)

{-toString :: FIXTag -> ByteString-}
{-toString = C.pack . show -}

equals :: ByteString
equals = C.singleton '='
                 
-- externalize the FIXMessage
coparse :: FIXMessage -> ByteString
coparse l = message' `append` checksum' `C.snoc` fixDelimiter
	where 
		message' = header `append` length' `C.snoc` fixDelimiter `append` body'
		checksum' = checksumTag `append` equals `append` paddedChecksum message'
		length' = lengthTag `append` equals `append` C.pack (show $ C.length body')
		body' = body l `C.snoc` fixDelimiter
