module Common.FIXTag where

import Common.FIXMessage ( FIXTag(..) )
import Common.FIXParserCombinators
import Common.FIXParser ( toFIXString, toFIXInt, toFIXUTCTimestamp )
import qualified Data.LookupTable as LT
import Data.IntMap (IntMap)
import Data.Maybe (fromMaybe)

