import Common.FIXCoparser
import Common.FIXMessage
import Data.ByteString.Char8 as C

input :: [FIXMessage]
input = [(FIX_VERSION, FIXString (C.pack "FIX.4.1")), (FIX_VERSION, FIXString (C.pack "FIX.4.1"))]
test = coparse input
