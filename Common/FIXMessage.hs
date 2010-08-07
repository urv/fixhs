module Common.FIXMessage 
	where

import Data.HashTable
import Data.ByteString

--  TODO: add missing fields
--  probably generate the complete list
--  or try something with FIXML...?
data FixTag = NA0
	| NA1
	| NA2
	| NA3
	| NA4
	| NA5
	| NA6
	| NA7
	| FIX_VERSION -- 8
	| FIX_MSG_LENGTH -- 9
	| FIX_CHECKSUM -- 10
	deriving (Show, Eq, Enum)
                 
                 
type FIXBody = HashTable FixTag FIXValue

data FIXValue = FIXInt Int | FIXBool Bool | FIXString ByteString deriving (Show, Eq)
