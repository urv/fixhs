import Prelude as P
import Common.FIXMessage
import Data.ByteString as B
import Data.ByteString.Char8 as C

-- implementation is not efficient 
-- this is just meant for testing
externalize :: FIXMessage -> ByteString
externalize (t,v) = C.pack $ (show t ++ ":" ++ show v) 

coparse :: [FIXMessage] -> ByteString
coparse l = B.concat $ P.map externalize l
