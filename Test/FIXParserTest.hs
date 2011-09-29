{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Data.FIX.Arbitrary
import qualified Data.LookupTable as LT
import Common.FIXMessage ( FIXSpec(..), FIXMessage(..), FIXValue(..), FIXValues(..) )
import Common.FIXCoparser ( coparse )
import Common.FIXParser ( _nextP', messageP  )
import Data.Attoparsec ( parseOnly )
import Data.List (group)
import Test.QuickCheck ( sample, sample', Gen, oneof, quickCheck, forAll, collect )
import Data.FIX.FIX42
import Data.ByteString ( ByteString )


prop_Idempotent xs = 
	collect (mType xs) $ 
	coparse xs == ((coparse $ parse $ coparse $ parse $ coparse xs) :: ByteString)
	where
	   types = xs :: FIXMessage FIXSpec
	   fixSpec = mContext xs
           parse ss = case parseOnly (_nextP' >>= messageP fixSpec) ss of
	   	Left err -> error err
		Right ms -> ms
	   	
messagesOf :: FIXSpec -> Gen (FIXMessage FIXSpec)
messagesOf spec = oneof $ map (arbitraryFIXMessage spec) allMessages 
	where
	   allMessages = map snd $ LT.toList $ fsMessages spec

test = quickCheck $ forAll (messagesOf fix42) prop_Idempotent

instance Eq FIXValue where
	FIXInt left == FIXInt right = left == right
	FIXInt _ == _ = False

	FIXDouble left == FIXDouble right = left == right
	FIXDouble _ == _ = False

	FIXChar left == FIXChar right = left == right
	FIXChar _ == _ = False

	FIXBool left == FIXBool right = left == right
	FIXBool _ == _ = False

	FIXString left == FIXString right = left == right
	FIXString _ == _ = False

	FIXData left == FIXData right = left == right
	FIXData _ == _ = False

	FIXMultipleValueString left == FIXMultipleValueString right = left == right
	FIXMultipleValueString _ == _ = False

	FIXTimestamp left == FIXTimestamp right = left == right
	FIXTimestamp _ == _ = False

	FIXTimeOnly left == FIXTimeOnly right = left == right
	FIXTimeOnly _ == _ = False

	FIXDateOnly left == FIXDateOnly right = left == right
	FIXDateOnly _ == _ = False

	FIXMonthYear left == FIXMonthYear right = left == right
	FIXMonthYear _ == _ = False

	FIXGroup nleft left == FIXGroup nright right = 
		nleft == nright && left == right 
	FIXGroup _ _ == _ = False

-- Define a new type Set that is an unordered List
newtype Set a = Set [a]

isSubset :: Eq a => [a] -> [a] -> Bool
isSubset xs ys = foldr ((&&) . isElement) True $ group ys
	where
           isElement = flip elem $ group xs

instance Eq a => Eq (Set a) where
	Set left == Set right = 
		length left == length right &&
		isSubset left right && isSubset right left 

instance Eq FIXValues where
	left == right = Set (LT.toList left) == Set (LT.toList right)

instance Eq (FIXMessage a) where
	left == right = mType left == mType right 
		&& mHeader left == mHeader right 
		&& mBody left == mBody right 
		&& mTrailer left == mTrailer right

instance Show (FIXMessage FIXSpec) where
	show ms = coparse ms
