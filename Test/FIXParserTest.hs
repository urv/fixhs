-- import Prelude hiding ( take, null, head, tail )
-- import Common.FIXMessage
-- import Common.FIXParserCombinators
import Data.Attoparsec hiding ( takeWhile1 )
-- import Data.Char
import Data.ByteString hiding ( pack, putStrLn, take )
import Common.FIXParser
import Data.ByteString.Char8 ( pack, readInt )
-- import Control.Applicative ( (<$>) )


input1 :: ByteString
input1 = pack "8=FIX.4.1|9=73|35=0|49=BRKR|56=INVMGR|34=235|52=19980604-07:58:28|112=19980604-07:58:28|10=230|"

input2 :: ByteString
input2 = pack "35=0|49=BRKR|56=INVMGR|34=235|52=19980604-07:58:28|112=19980604-07:58:28|"

input3 :: ByteString
input3 = pack "8=FIX.4.2|9=145|35=D|34=4|49=ABC_DEFG01|52=20090323-15:40:29|56=CCG|115=XYZ|11=NF 0542/03232009|54=1|38=100|55=CVS|40=1|59=0|47=A|60=20090323-15:40:29|21=1|207=N|10=112|"


process = do case parse messageParser input3 of 
               Done m r -> parseMessageBody r
               _ -> undefined

-- here
parseMessageBody r = do case parse bodyParser r of
                          Partial f -> f empty
                          _ -> undefined
