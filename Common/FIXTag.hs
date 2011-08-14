module Common.FIXTag where

import Common.FIXMessage ( FIXTag(..) )
import Common.FIXParserCombinators
import qualified Data.LookupTable as LT
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

versionFIXTag   = FIXTag 8  toFIXString
msgLengthFIXTag = FIXTag 9  toFIXInt
checkSumFIXTag  = FIXTag 10 toFIXInt
dummyTag        = FIXTag 12 toFIXString

tagLookupTable :: IntMap FIXTag
tagLookupTable = LT.insert 9 msgLengthFIXTag $ 
                 LT.insert 10 checkSumFIXTag $ 
                 LT.insert 8 versionFIXTag LT.new     

toFIXTag :: Int -> FIXTag
toFIXTag i = case LT.lookup i tagLookupTable of
                Just t  -> t
                Nothing -> dummyTag
