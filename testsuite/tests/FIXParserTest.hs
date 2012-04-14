{-# LANGUAGE 
    TupleSections
  , FlexibleInstances
  , TypeSynonymInstances #-}

import Data.FIX.Arbitrary
import qualified Data.LookupTable as LT
import Data.FIX.Message ( FIXGroupElement(..), FIXSpec(..), FIXMessage(..), FIXValue(..), FIXValues(..), FIXTag(..) )
import Data.FIX.Coparser ( coparse )
import Data.FIX.Parser ( nextP, nextP', messageP  )
import Data.Attoparsec ( parseOnly )
import Data.List (group)
import Test.QuickCheck ( (==>), sample, sample', Gen, oneof, quickCheck, quickCheckResult, forAll, collect, Result(..) )
import Data.FIX.Spec.FIX42
import qualified Data.FIX.Common as FIX
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C ( singleton, append )
import System.Time ( CalendarTime(..) )
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( void )

prop_orthogonal xs = 
	collect (mType xs) $ 
	xs == parse (coparse xs)
	where
	   types = xs :: FIXMessage FIXSpec
	   fixSpec = mContext xs
           parse ss = case parseOnly (nextP' >>= messageP fixSpec) ss of
	   	Left err -> error err
		Right ms -> ms
	   	
messagesOf :: FIXSpec -> Gen (FIXMessage FIXSpec)
messagesOf spec = oneof $ map (arbitraryFIXMessage spec) allMessages 
	where
	   allMessages = map snd $ LT.toList $ fsMessages spec

tagsOf :: FIXSpec -> Gen (FIXTag, FIXValue)
tagsOf spec = oneof $ map arbitraryTag allTags
	where
	   allTags = map snd $ LT.toList $ fsTags spec
	   arbitraryTag t = fmap (t,) $ arbitraryValue t

prop_tag (tag, v) = collect (tName tag) $ 
	v == parse (coparse v)
	where
           parse ss = let ss' = ss `C.append` C.singleton FIX.delimiter in 
	   	case parseOnly (tparser tag) ss' of
			Left err -> error err
			Right ms -> ms

instance Eq FIXGroupElement where
	FIXGroupElement i1 s1 vs1 == FIXGroupElement i2 s2 vs2 = 
		i1 == i2 && s1 == s2 && vs1 == vs2 

instance Eq FIXValue where
	FIXInt left == FIXInt right = left == right
	FIXInt _ == _ = False

	FIXDouble left == FIXDouble right = 
		left >= right - 0.5
		&& left <= right + 0.5
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

	FIXTimestamp left == FIXTimestamp right = 
		ctYear left == ctYear right 
		&& ctMonth left == ctMonth right 
		&& ctDay left == ctDay right 
		&& ctHour left == ctHour right 
		&& ctMin left == ctMin right 
		&& ctSec left == ctSec right 
		{-&& ctPicosec left == ctPicosec right-}
	FIXTimestamp _ == _ = False

	FIXTimeOnly left == FIXTimeOnly right = 
		ctHour left == ctHour right 
		&& ctMin left == ctMin right 
		&& ctSec left == ctSec right 
		{-ctPicosec left == ctPicosec right-}
	FIXTimeOnly _ == _ = False

	FIXDateOnly left == FIXDateOnly right = 
		ctYear left == ctYear right 
		&& ctMonth left == ctMonth right 
		&& ctDay left == ctDay right
	FIXDateOnly _ == _ = False

	FIXMonthYear left == FIXMonthYear right = 
		ctYear left == ctYear right 
		&& ctMonth left == ctMonth right
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

instance Show FIXTag where
	show = tName

instance Show FIXValue where
	show (FIXInt i) = show i
	show (FIXDouble i) = show i
	show (FIXChar i) = show i
	show (FIXBool i) = show i
	show (FIXString i) = show i
	show (FIXData i) = show i
	show (FIXMultipleValueString i) = show i
	show (FIXTimestamp i) = show i
	show (FIXTimeOnly i) = show i
	show (FIXDateOnly i) = show i
	show (FIXMonthYear i) = show i

instance Show (FIXMessage FIXSpec) where
	show ms = show (coparse ms :: ByteString)


main = do
	check $ quickCheckResult $ forAll (messagesOf fix42) prop_orthogonal
	check $ quickCheckResult $ forAll (tagsOf fix42) prop_tag
	where
		check :: IO Result -> IO ()
		check io = do 
			res <- io
			case res of 
				Success {} -> return ()
				_ -> void (exitWith (ExitFailure 1)) 
