-- Module  : Data.LookupTable
-- License : LGPL-2.1 

{-# LANGUAGE FunctionalDependencies, ExistentialQuantification, MultiParamTypeClasses, FlexibleInstances #-}

module Data.LookupTable 
    ( LookupTable
    , new
    , lookup
    , insert
    , toList
    , fromList 
    ) where

import Prelude hiding (lookup)

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

class Ord k => LookupTable k v t 
        | t -> k
        , t -> v 
    where
    new :: t
    lookup :: k -> t -> Maybe v
    insert :: k -> v -> t -> t
    toList :: t -> [(k, v)]
    fromList :: [(k, v)] -> t

instance Ord k => LookupTable k v (Map k v) where 
    new      = Map.empty
    lookup   = Map.lookup
    insert   = Map.insert
    toList   = Map.toList
    fromList = Map.fromList

instance Ord k => LookupTable k v [(k,v)] where
    new      = []
    lookup   = List.lookup  
    insert   = curry (:)
    toList   = id
    fromList = id

instance LookupTable Int v (IntMap v) where
    new      = IntMap.empty
    lookup   = IntMap.lookup
    insert   = IntMap.insert
    toList   = IntMap.toList
    fromList = IntMap.fromList
