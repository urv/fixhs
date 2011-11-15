module Data.FIX.Common
    ( delimiter)
    where

import Data.ByteString ( ByteString )
import Data.ByteString.Char8 as C ( pack )
 
delimiter :: Char
delimiter = '\SOH'
