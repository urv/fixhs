import Common.FIXParser
import Common.FIXCoparser
import Data.ByteString.Char8 ( pack, readInt )
import Data.Attoparsec

input1 = pack "8=FIX.4.2\SOH9=65\SOH35=A\SOH49=SERVER\SOH56=CLIENT\SOH34=177\SOH52=20090107-18:15:16\SOH98=0\SOH108=30\SOH10=062\SOH"
output1 = case parseMessage input1 of
         Done m r -> coparse r
         _ -> undefined

test1 = (show input1) == (show output1)
