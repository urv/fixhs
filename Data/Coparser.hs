{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Data.Coparser 
    ( Coparser (..)
    , TextLike (..)
    ) where

import Data.ByteString ( ByteString )
import qualified Data.ByteString as B ( unpack, append, singleton )
import qualified Data.ByteString.Char8 as C 
    ( cons, snoc, pack, unpack, singleton )
import Data.Text.Lazy.Builder ( Builder )
import qualified Data.Text.Lazy as Builder ( unpack )
import qualified Data.Text.Lazy.Builder as Builder 
    ( fromString, singleton, toLazyText )
import Data.Monoid ( mappend )
import Control.DeepSeq ( NFData (..) )

class TextLike a where
    pack :: String -> a
    unpack :: a -> String

    singleton :: Char -> a
    append :: a -> a -> a

    cons :: Char -> a -> a
    cons c t = singleton c `append` t

    snoc :: a -> Char -> a
    snoc t c = t `append` singleton c


instance TextLike String where
    pack = id
    unpack = id
    singleton c = [c]
    append = (++)
    cons = (:)

instance TextLike ByteString where
    pack = C.pack
    unpack = C.unpack
    singleton = C.singleton
    append = B.append
    cons = C.cons
    snoc = C.snoc
    
instance TextLike Builder where
    pack = Builder.fromString
    unpack = Builder.unpack . Builder.toLazyText
    singleton = Builder.singleton
    append = mappend

instance NFData Builder where
    rnf = rnf . Builder.unpack . Builder.toLazyText

class Coparser a where
    coparse :: TextLike t => a -> t
