module Common.FIXTag where

import Common.FIXMessage ( FIXTag(..) )
import Common.FIXParserCombinators
import qualified Data.LookupTable as LT
import Data.IntMap (IntMap)
import Data.Maybe (fromMaybe)

dummyTag :: FIXTag
dummyTag = FIXTag 12 toFIXString

tagLookupTable :: IntMap FIXTag
tagLookupTable = LT.insert 8 (FIXTag 8  toFIXString) $ 
                 LT.insert 9 (FIXTag 9  toFIXInt) $ 
                 LT.insert 52 (FIXTag 52  toFIXUTCTimestamp) $ 
                 LT.insert 10 (FIXTag 10 toFIXInt) LT.new     

toFIXTag :: Int -> FIXTag
toFIXTag i = fromMaybe dummyTag (LT.lookup i tagLookupTable)
