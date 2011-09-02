module Data.FIX.Common
    ( delimiter
    , fixVersion )
    where


import Data.ByteString ( ByteString )
import Data.ByteString.Char8 as C ( pack )
 
delimiter :: Char
delimiter = '\SOH'

fixVersion :: ByteString
fixVersion = C.pack "8=FIX.4.2"
