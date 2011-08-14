{-# Language ExistentialQuantification, FlexibleContexts #-}

module Common.FIXMessage 
	where

import System.Time ( CalendarTime )
import Prelude hiding ( take, null, head, tail, length )
import Data.HashTable
import Data.ByteString 
import Data.ByteString.Char8 as C hiding ( take, null, head, tail, length )
import Data.LookupTable 

--  TODO: add missing fields
--  probably generate the complete list
--  or try something with FIXML...?
data FIXTag = NA0
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
	| NA11 
	| NA12 
	| NA13 
	| NA14 
	| NA15 
	| NA16 
	| NA17 
	| NA18 
	| NA19 
	| NA20 
	| NA21 
	| NA22 
	| NA23 
	| NA24 
	| NA25 
	| NA26 
	| NA27 
	| NA28 
	| NA29 
	| NA30 
	| NA31 
	| NA32 
	| NA33 
	| NA34 
	| NA35 
	| NA36 
	| NA37 
	| NA38 
	| NA39 
	| NA40 
	| NA41 
	| NA42 
	| NA43 
	| NA44 
	| NA45 
	| NA46 
	| NA47 
	| NA48 
	| NA49 
	| NA50 
	| NA51 
	| NA52 
	| NA53 
	| NA54 
	| NA55 
	| NA56 
	| NA57 
	| NA58 
	| NA59 
	| NA60 
	| NA61 
	| NA62 
	| NA63 
	| NA64 
	| NA65 
	| NA66 
	| NA67 
	| NA68 
	| NA69 
	| NA70 
	| NA71 
	| NA72 
	| NA73 
	| NA74 
	| NA75 
	| NA76 
	| NA77 
	| NA78 
	| NA79 
	| NA80 
	| NA81 
	| NA82 
	| NA83 
	| NA84 
	| NA85 
	| NA86 
	| NA87 
	| NA88 
	| NA89 
	| NA90 
	| NA91 
	| NA92 
	| NA93 
	| NA94 
	| NA95 
	| NA96 
	| NA97 
	| NA98 
	| NA99 
	| NA100 
	| NA101 
	| NA102 
	| NA103 
	| NA104 
	| NA105 
	| NA106 
	| NA107 
	| NA108 
	| NA109 
	| NA110 
	| NA111 
	| NA112 
	| NA113 
	| NA114 
	| NA115 
	| NA116 
	| NA117 
	| NA118 
	| NA119 
	| NA120 
	| NA121 
	| NA122 
	| NA123 
	| NA124 
	| NA125 
	| NA126 
	| NA127 
	| NA128 
	| NA129 
	| NA130 
	| NA131 
	| NA132 
	| NA133 
	| NA134 
	| NA135 
	| NA136 
	| NA137 
	| NA138 
	| NA139 
	| NA140 
	| NA141 
	| NA142 
	| NA143 
	| NA144 
	| NA145 
	| NA146 
	| NA147 
	| NA148 
	| NA149 
	| NA150 
	| NA151 
	| NA152 
	| NA153 
	| NA154 
	| NA155 
	| NA156 
	| NA157 
	| NA158 
	| NA159 
	| NA160 
	| NA161 
	| NA162 
	| NA163 
	| NA164 
	| NA165 
	| NA166 
	| NA167 
	| NA168 
	| NA169 
	| NA170 
	| NA171 
	| NA172 
	| NA173 
	| NA174 
	| NA175 
	| NA176 
	| NA177 
	| NA178 
	| NA179 
	| NA180 
	| NA181 
	| NA182 
	| NA183 
	| NA184 
	| NA185 
	| NA186 
	| NA187 
	| NA188 
	| NA189 
	| NA190 
	| NA191 
	| NA192 
	| NA193 
	| NA194 
	| NA195 
	| NA196 
	| NA197 
	| NA198 
	| NA199 
	| NA200 
	| NA201 
	| NA202 
	| NA203 
	| NA204 
	| NA205 
	| NA206 
	| NA207 
	deriving (Show, Eq, Enum, Ord)

data ListOfFIXTokens = forall t . LookupTable Int FIXValue t => LT t
type FIXBody = ListOfFIXTokens
type FIXHeader = ListOfFIXTokens
data FIXValue = FIXInt Int 
              | FIXDayOfMonth Int
              | FIXFloat Float
              | FIXQuantity Float
              | FIXPrice Float
              | FIXPriceOffset Float
              | FIXAmt Float
              | FIXChar Char 
              | FIXBool Bool 
              | FIXString ByteString 
              | FIXMultipleValueString ByteString 
              | FIXCurrency ByteString 
              | FIXExchange ByteString 
              | FIXUTCTimestamp CalendarTime
              | FIXUTCTimeOnly CalendarTime
              | FIXLocalMktDate CalendarTime
              | FIXUTCDate CalendarTime
              | FIXMonthYear CalendarTime
              | FIXData { dataLen :: Int, dataChunk :: ByteString }
              | FIXGroup FIXMessage

              deriving (Show, Eq)

type FIXMessage = [(FIXTag, FIXValue)]

fixDelimiter :: Char
fixDelimiter = '\SOH'

fixVersion :: ByteString
fixVersion = C.pack "8=FIX.4.2"

paddedChecksum :: ByteString -> ByteString
paddedChecksum = checksum' . checksum

-- FIX checksum is simply the sum of bytes modulo 256
checksum :: ByteString -> Int
checksum b | null b = 0
           | otherwise = (fromIntegral (head b) + checksum (tail b)) `mod` 256       

-- FIX length
checksum' :: Int -> ByteString
checksum' b | b < 10 = C.pack "00" `append` num
            | b < 100 = C.cons '0' num
	        | otherwise = num
            where num = C.pack (show b)
