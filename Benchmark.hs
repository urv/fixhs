{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main
import Criterion.Config
import Data.FIX.FIX42
import Common.FIXParser
import Common.FIXMessage
import Data.Attoparsec hiding (take)
import qualified Data.ByteString.Char8 as C
import Control.DeepSeq

input = C.pack "8=FIX.4.2\SOH9=178\SOH35=8\SOH49=PHLX\SOH56=PERS\SOH52=20071123-05:30:00.000\SOH11=ATOMNOCCC9990900\SOH20=3\SOH150=E\SOH39=E\SOH55=MSFT\SOH167=CS\SOH54=1\SOH38=15\SOH40=2\SOH44=15\SOH58=PHLX EQUITY TESTING\SOH59=0\SOH47=C\SOH32=0\SOH31=0\SOH151=15\SOH14=0\SOH6=0\SOH10=128\SOH"

myConfig = defaultConfig 

main = defaultMainWith myConfig (return ()) [ 
         bgroup "FIX42" [ 
            bench "ExecutionReport" $ nf (parseOnly (nm fix42)) input 
         ]
       ]
