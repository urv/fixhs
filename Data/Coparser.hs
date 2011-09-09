{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances #-}

-- Module   : Common.FIXCoparser
-- License  : GPLv2

module Data.Coparser 
    ( Coparser (..)
    , BuilderLike (..)
    , length
    , Data.Coparser.foldl'
    , Data.Coparser.foldl
    ) where

import Prelude hiding ( length ) 
import qualified Prelude as P ( length, foldl )
import qualified Data.Foldable as P ( foldl' )
import qualified Data.List as L ( concat )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B ( concat, append  )
import Data.Word ( Word8 )
import qualified Data.ByteString.Char8 as C 
    ( cons, snoc, pack, unpack, singleton )
import Data.Text.Lazy.Builder ( Builder )
import qualified Data.Text.Lazy as Builder ( unpack )
import qualified Data.Text.Lazy.Builder as Builder 
    ( fromString, singleton, toLazyText )
import Data.Monoid ( mappend, mconcat )
import Control.DeepSeq ( NFData (..) )
import qualified Data.DList as DL

class Enum c => BuilderLike a c | a -> c where
    pack :: String -> a
    unpack :: a -> String
    singleton :: Char -> a
    append :: a -> a -> a
    concat :: [a] -> a
    cons :: Char -> a -> a
    snoc :: a -> Char -> a
    decimal :: Integral i => i -> a
    realFloat :: RealFloat r => r -> a

    decimal = pack . show
    realFloat = pack . show
    cons c t = singleton c `append` t
    snoc t c = t `append` singleton c


length :: (BuilderLike a c) => a -> Int
length = P.length . unpack

foldl' :: (BuilderLike a c) => (b -> Char -> b) -> b -> a -> b
foldl' f x0 = P.foldl' f x0 . unpack

foldl :: (BuilderLike a c) => (b -> Char -> b) -> b -> a -> b
foldl f x0 = P.foldl f x0 . unpack

instance BuilderLike String Char where
    pack = id
    unpack = id
    singleton c = [c]
    append = (++)
    cons = (:)
    concat = L.concat

instance BuilderLike ByteString Word8 where
    pack = C.pack
    unpack = C.unpack
    singleton = C.singleton
    append = B.append
    cons = C.cons
    snoc = C.snoc
    concat = B.concat

instance BuilderLike (DL.DList Char) Char where
    pack = DL.fromList
    unpack = DL.toList
    singleton = DL.singleton
    append = DL.append
    cons = DL.cons
    snoc = DL.snoc
    concat = DL.concat
    
instance BuilderLike Builder Char where
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
    coparse :: BuilderLike t c => a -> t
