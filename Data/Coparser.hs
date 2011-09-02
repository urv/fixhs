module Data.Coparser 
    ( Coparser (..)
    ) where

import Data.ByteString ( ByteString )

class Coparser a where
    coparse :: a -> ByteString
