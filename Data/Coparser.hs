{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Data.Coparser 
    ( Coparser (..)
    , TextLike (..)
    , length
    , Data.Coparser.foldl'
    , Data.Coparser.foldl
    ) where

import Prelude hiding ( length ) 
import qualified Prelude as P ( length, foldl )
import qualified Data.Foldable as P ( foldl' )
import qualified Data.List as L ( concat )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B 
    ( length, concat, unpack, append, singleton, foldl, foldl' )
import Data.Word ( Word8 )
import qualified Data.ByteString.Char8 as C 
    ( cons, snoc, pack, unpack, singleton )
import Data.Text.Lazy.Builder ( Builder )
import qualified Data.Text.Lazy as Builder ( unpack, foldl, foldl' )
import qualified Data.Text.Lazy.Builder as Builder 
    ( fromString, singleton, toLazyText )
import Data.Monoid ( mappend, mconcat )
import Control.DeepSeq ( NFData (..) )
import qualified Data.DList as DL

class Enum c => TextLike a c | a -> c where
    pack :: String -> a
    unpack :: a -> String
    singleton :: Char -> a
    append :: a -> a -> a
    concat :: [a] -> a
    cons :: Char -> a -> a
    snoc :: a -> Char -> a

    cons c t = singleton c `append` t
    snoc t c = t `append` singleton c


length :: (TextLike a c) => a -> Int
length = P.length . unpack

foldl' :: (TextLike a c) => (b -> Char -> b) -> b -> a -> b
foldl' f x0 = P.foldl' f x0 . unpack

foldl :: (TextLike a c) => (b -> Char -> b) -> b -> a -> b
foldl f x0 = P.foldl f x0 . unpack

instance TextLike String Char where
    pack = id
    unpack = id
    singleton c = [c]
    append = (++)
    cons = (:)
    concat = L.concat

instance TextLike ByteString Word8 where
    pack = C.pack
    unpack = C.unpack
    singleton = C.singleton
    append = B.append
    cons = C.cons
    snoc = C.snoc
    concat = B.concat

instance TextLike (DL.DList Char) Char where
    pack = DL.fromList
    unpack = DL.toList
    singleton = DL.singleton
    append = DL.append
    cons = DL.cons
    snoc = DL.snoc
    concat = DL.concat
    
instance TextLike Builder Char where
    pack = Builder.fromString
    unpack = Builder.unpack . Builder.toLazyText
    singleton = Builder.singleton
    append = mappend
    concat = mconcat

instance NFData Builder where
    rnf = rnf . Builder.unpack . Builder.toLazyText

instance NFData (DL.DList Char) where
    rnf = rnf . unpack

class Coparser a where
    coparse :: TextLike t c => a -> t
