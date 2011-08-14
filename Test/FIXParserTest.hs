import Prelude hiding ( take, null, head, tail )
import Data.Attoparsec hiding ( takeWhile1 )
import Data.ByteString hiding ( pack, putStrLn, take, map )
import Common.FIXParser
import Common.FIXMessage
import Data.ByteString.Char8 ( pack, readInt )
import Control.Applicative ( (<$>) )

input1 :: ByteString
input1 = pack "8=FIX.4.2\SOH9=65\SOH35=A\SOH49=SERVER\SOH56=CLIENT\SOH34=177\SOH52=20090107-18:15:16\SOH98=0\SOH108=30\SOH10=062\SOH"

input2 :: ByteString
input2 = pack "8=FIX.4.2\SOH9=178\SOH35=8\SOH49=PHLX\SOH56=PERS\SOH52=20071123-05:30:00.000\SOH11=ATOMNOCCC9990900\SOH20=3\SOH150=E\SOH39=E\SOH55=MSFT\SOH167=CS\SOH54=1\SOH38=15\SOH40=2\SOH44=15\SOH58=PHLX EQUITY TESTING\SOH59=0\SOH47=C\SOH32=0\SOH31=0\SOH151=15\SOH14=0\SOH6=0\SOH10=128\SOH"
  
test = parseOnly messageParser
