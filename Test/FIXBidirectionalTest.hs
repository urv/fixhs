import Common.FIXParser
import Common.FIXMessage
import Common.FIXCoparser
import Data.ByteString.Char8 ( pack )
import Data.Attoparsec
import Test.QuickCheck
import Data.Char

-- test 1: parse sample fix message and externalize it again
input1 = pack "8=FIX.4.2\SOH9=65\SOH35=A\SOH49=SERVER\SOH56=CLIENT\SOH34=177\SOH52=20090107-18:15:16\SOH98=0\SOH108=30\SOH10=062\SOH"
output1 = case parseMessage input1 of
         Done m r -> coparse r
         _ -> undefined

test1 = (show input1) == (show output1)

-- test 2: build fix message, externalize, parse, externalize
input2' :: [FIXMessage]
input2' = [(NA49, FIXString (pack "xyz")), (NA49, FIXString (pack "abc"))]

input2 = coparse input2'
output2 = case parseMessage input2 of
         Done m r -> coparse r
         _ -> undefined

test2 = (show input2) == (show output2)

-- test 3: build fix message, externalize, parse, externalize
input3' :: String -> String -> [FIXMessage]
input3' a b = [(NA49, FIXString (pack a)), (NA49, FIXString (pack b))]

input3 a b = coparse (input3' a b)
output3 a b = case parseMessage (input3 a b) of
         	Done m r -> coparse r
         	_ -> undefined

test3' x y = (show $ input3 x y) == (show $ output3 x y)
test3'' x y = isF x && isF y ==> test3' x y
test3 = quickCheck test3''

isF x = isA x && isS x
	where
		isA = Prelude.any isAscii
		isS = Prelude.all (/=fix_delimiter) 
