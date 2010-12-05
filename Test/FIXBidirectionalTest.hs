import Prelude hiding ( putStr )

import Common.FIXParser
import Common.FIXMessage
import Common.FIXCoparser
import Data.ByteString hiding ( pack, map )
import Data.ByteString.Char8 ( pack )
import Data.Attoparsec 
import Test.QuickCheck hiding ( Result )
import Data.Char

hylo :: ByteString -> ByteString
hylo input = case parseMessage input of
         	Done m r -> case r of
	 		Done n s -> coparse s

-- test 1: parse sample fix message and externalize it again
input1 = pack "8=FIX.4.2\SOH9=65\SOH35=A\SOH49=SERVER\SOH56=CLIENT\SOH34=177\SOH52=20090107-18:15:16\SOH98=0\SOH108=30\SOH10=062\SOH"
output1 = hylo input1

test1 = (show input1) == (show output1)

-- test 2: wrong checksum
input2 = pack "8=FIX.4.2\SOH9=65\SOH35=A\SOH49=SERVER\SOH56=CLIENT\SOH34=177\SOH52=20090107-18:15:16\SOH98=0\SOH108=30\SOH10=063\SOH8=FIX.4.2\SOH9=65\SOH35=A\SOH49=SERVER\SOH56=CLIENT\SOH34=177\SOH52=20090107-18:15:16\SOH98=0\SOH108=30\SOH10=062\SOH"
output2 = case parseMessage input2 of
		Fail _ _ s -> s

test2 = output2 == "Failed reading: checksum not valid"

-- test 3: build fix message, externalize, parse, externalize
input3 a b = coparse (input3' a b)
	where input3' a b = [(NA49, FIXString (pack a)), (NA49, FIXString (pack b))]
output3 a b = hylo (input3 a b)

test3 = quickCheck test3''
	where test3' x y = (show $ input3 x y) == (show $ output3 x y)
	      test3'' x y = isF x && isF y ==> test3' x y
	      isF x = isA x && isS x
	      isA = Prelude.any isAscii
	      isS = Prelude.all (/=fix_delimiter) 

-- test 4: multiple messages
input4 = input1 `append` input1
output4 = hylo input4

