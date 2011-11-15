-- Module   : Data.FIX.Message
-- License  : GPLv2

{-# LANGUAGE MagicHash, GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

-- | FIX messages
module Data.FIX.Message 
    ( FIXValue (..)
    , FIXValues
    , FIXTag (..)
    , FIXTags
    , FIXMessages
    , FIXMessage (..)
    , FIXMessageSpec (..)
    , FIXGroupSpec (..)
    , FIXGroupElement (..)
    , FIXSpec (..)
    , checksum
    , delimiter
    ) where

import System.Time ( CalendarTime (..) )
import Prelude hiding ( null )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B ( null, foldl' )
import Data.IntMap ( IntMap, elems )
import Data.Map ( Map )
import Data.Attoparsec ( Parser ) 
import Data.LookupTable ( LookupTable )
import qualified Data.LookupTable as LT ( toList )
import Control.DeepSeq ( NFData (..) )
import Test.QuickCheck ( Gen )
import Data.FIX.Common ( delimiter )
import Data.Coparser ( BuilderLike (..), foldl' )

-- | A valid FIX field description. It is used to specify FIX messages using
-- 'FIXMessageSpec'.
data FIXTag = FIXTag 
    { tName :: String 
    -- ^ The name of the tag e.g. BeginString.
    , tnum :: Int 
    -- ^ The numerical value of the tag e.g. 8.
    , tparser :: Parser FIXValue 
    -- ^ The corresponding attoparsec parser.
    , arbitraryValue :: Gen FIXValue 
    -- ^ A random generator for that particular types of fields.
    } 


-- | 
data FIXGroupElement = FIXGroupElement Int FIXValue FIXValues

data FIXValue = FIXInt Int 
              | FIXDouble Double
              | FIXChar Char 
              | FIXBool Bool 
              | FIXString ByteString 
              | FIXData ByteString 
              | FIXMultipleValueString ByteString 
              | FIXTimestamp CalendarTime
              | FIXTimeOnly CalendarTime
              | FIXDateOnly CalendarTime
              | FIXMonthYear CalendarTime
              | FIXGroup Int [FIXGroupElement]

--- should be alias of type in the typeclass LookupTable
newtype ListOfValues a = LoV (IntMap a) 
    deriving (LookupTable Int a, NFData)

instance Show a => Show (ListOfValues a) where
    show a = concatMap printKV $ LT.toList a
        where
            printKV (k, v) = show k ++ " = " ++ show v ++ "\n"

type FIXValues = ListOfValues FIXValue 
data FIXMessage a = FIXMessage
                  { mContext :: a
                  , mType    :: ByteString
                  , mHeader  :: FIXValues
                  , mBody    :: FIXValues
                  , mTrailer :: FIXValues }


newtype ListOfTags a = LoT (IntMap a)
    deriving (LookupTable Int a)

type FIXTags = ListOfTags FIXTag
data FIXMessageSpec = FMSpec 
                      { msName    :: String
                      , msType    :: ByteString
                      , msHeader  :: FIXTags
                      , msBody    :: FIXTags 
                      , msTrailer :: FIXTags }

newtype ListOfMessages a = LM (Map ByteString a)
    deriving (LookupTable ByteString a)

type FIXMessages = ListOfMessages FIXMessageSpec
data FIXSpec = FSpec 
               { fsVersion  :: String       -- ^ FIX version
               , fsHeader   :: FIXTags      -- ^ FIX header tags 
               , fsTrailer  :: FIXTags      -- ^ FIX trailer tags
               , fsMessages :: FIXMessages  -- ^ Dictionary of all FIX messages
               , fsTags     :: FIXTags      -- ^ Dictionary of all FIX tags
               }    

data FIXGroupSpec = FGSpec
                    { gsLength    :: FIXTag
                    , gsSeperator :: FIXTag 
                    , gsBody      :: FIXTags }


-- FIX checksum is simply the sum of bytes modulo 256
checksum :: (BuilderLike t c, Enum c)  => t -> Int
checksum b = foldl' _sumUp 0 b `mod` 256
                where 
                    _sumUp :: (Enum c) => Int -> c -> Int
                    _sumUp t c = t + fromEnum c

instance Control.DeepSeq.NFData ByteString 
instance Control.DeepSeq.NFData CalendarTime
instance Control.DeepSeq.NFData FIXGroupElement where
    rnf (FIXGroupElement _ s vs) = rnf s `seq` rnf vs 

instance Control.DeepSeq.NFData FIXValue where
    rnf (FIXInt x) = rnf x
    rnf (FIXDouble x) = rnf x
    rnf (FIXChar x) = rnf x
    rnf (FIXBool x) = rnf x
    rnf (FIXString x) = rnf x
    rnf (FIXMultipleValueString x) = rnf x
    rnf (FIXTimestamp x) = rnf x
    rnf (FIXTimeOnly x) = rnf x
    rnf (FIXDateOnly x) = rnf x
    rnf (FIXMonthYear x) = rnf x
    rnf (FIXData x) = rnf x 
    rnf (FIXGroup l es) = rnf l `seq` rnf es

instance Control.DeepSeq.NFData (FIXMessage a) where
    rnf (FIXMessage _ _ h b t) = rnf h `seq` rnf b `seq` rnf t

