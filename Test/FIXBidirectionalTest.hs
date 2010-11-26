import Common.FIXParser
import Common.FIXCoparser
import Data.ByteString.Char8 ( pack, readInt )

import Data.Attoparsec

input = pack "8=FIX.4.2|9=145|35=D|34=4|49=ABC_DEFG01|52=20090323-15:40:29|56=CCG|115=XYZ|11=NF 0542/03232009|54=1|38=100|55=CVS|40=1|59=0|47=A|60=20090323-15:40:29|21=1|207=N|10=112|"

input2 = pack "8=FIX.4.2|9=145|35=D|34=4|49=ABC_DEFG01|52=20090323-15:40:29|56=CCG|115=XYZ|11=NF 0542/03232009|54=1|38=100|55=CVS|40=1|59=0|47=A|60=20090323-15:40:29|21=1|207=N|10=112|8=FIX.4.2|9=145|35=D|34=4|49=ABC_DEFG01|52=20090323-15:40:29|56=CCG|115=XYZ|11=NF 0542/03232009|54=1|38=100|55=CVS|40=1|59=0|47=A|60=20090323-15:40:29|21=1|207=N|10=112|"

test1 = parseMessage input

output1 = case parseMessage input of
         Done m r -> coparse r
         _ -> undefined
