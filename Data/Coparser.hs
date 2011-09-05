{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

module Data.Coparser 
    ( Coparser (..)
    , TextLike (..)
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

class Enum c => TextLike a c | a -> c where
    pack :: String -> a
    unpack :: a -> String

    singleton :: Char -> a
    append :: a -> a -> a
    concat :: [a] -> a
    foldl :: (b -> c -> b) -> b -> a -> b
    foldl' :: (b -> c -> b) -> b -> a -> b

    cons :: Char -> a -> a
    cons c t = singleton c `append` t

    snoc :: a -> Char -> a
    snoc t c = t `append` singleton c

    length :: a -> Int


instance TextLike String Char where
    pack = id
    unpack = id
    singleton c = [c]
    append = (++)
    cons = (:)
    concat = L.concat
    foldl = P.foldl
    foldl' = P.foldl'
    length = P.length

instance TextLike ByteString Word8 where
    pack = C.pack
    unpack = C.unpack
    singleton = C.singleton
    append = B.append
    cons = C.cons
    snoc = C.snoc
    concat = B.concat
    foldl = B.foldl
    foldl' = B.foldl'
    length = B.length
    
instance TextLike Builder Char where
    pack = Builder.fromString
    unpack = Builder.unpack . Builder.toLazyText
    singleton = Builder.singleton
    append = mappend
    concat = mconcat
    foldl f x0 = Builder.foldl f x0 . Builder.toLazyText
    foldl' f x0  = Builder.foldl' f x0 . Builder.toLazyText
    length = P.length . unpack 

instance NFData Builder where
    rnf = rnf . Builder.unpack . Builder.toLazyText

class Coparser a where
    coparse :: TextLike t c => a -> t
