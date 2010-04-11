module Common.FIXMessage 
	where

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

