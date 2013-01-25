-- Module  : Data.LookupTable
-- License : LGPL-2.1 

{-# LANGUAGE 
    TypeFamilies
  , ExistentialQuantification
  , MultiParamTypeClasses
  , FlexibleInstances #-}

module Data.LookupTable ( LookupTable(..) ) 
    where

import Prelude hiding ( lookup )

import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.List as List
import Data.IntMap ( IntMap )
import qualified Data.IntMap as IntMap

class LookupTable t where
    type KeyOf   t :: *
    type ValueOf t :: *
    new      :: t
    lookup   :: (KeyOf t) -> t -> Maybe (ValueOf t)
    insert   :: (KeyOf t) -> (ValueOf t) -> t -> t
    toList   :: t -> [(KeyOf t, ValueOf t)]
    fromList :: [(KeyOf t, ValueOf t)] -> t

instance Ord k => LookupTable (Map k v) where 
    type KeyOf   (Map k v) = k
    type ValueOf (Map k v) = v
    new      = Map.empty
    lookup   = Map.lookup
    insert   = Map.insert
    toList   = Map.toList
    fromList = Map.fromList

instance Ord k => LookupTable [(k,v)] where
    type KeyOf   [(k,v)] = k
    type ValueOf [(k,v)] = v
    new      = []
    lookup   = List.lookup  
    insert   = curry (:)
    toList   = id
    fromList = id

instance LookupTable (IntMap v) where
    type KeyOf   (IntMap v) = Int
    type ValueOf (IntMap v) = v
    new      = IntMap.empty
    lookup   = IntMap.lookup
    insert   = IntMap.insert
    toList   = IntMap.toList
    fromList = IntMap.fromList
