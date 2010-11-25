import Common.FIXCoparser
import Common.FIXMessage
import Data.ByteString.Char8 as C

input :: [FIXMessage]
input = [(NA49, FIXString (C.pack "xyz")), (NA49, FIXString (C.pack "abc"))]
test = coparse input
