{-# LANGUAGE FlexibleContexts #-}
import Text.XML.HaXml
import System.Environment ( getArgs )
import qualified Data.LookupTable as LT
import Data.Map ( Map )
import Data.Maybe ( fromMaybe )

type AttrLookupTable = Map String String
type ParserLookupTable = Map String String

content :: Element a -> [Content a]
content (Elem _ _ cs) = cs

attributes :: Element a -> [Attribute]
attributes (Elem _ as _) = as

getFIXSpec :: Document a -> Maybe (Element a)
getFIXSpec d = case d of
    Document _ _ (es@(Elem (N n) _ _)) _ 
        -> if n == "fix" then Just es else Nothing
    _   -> Nothing 

getFieldSpec :: Document a -> Maybe (Element a)
getFieldSpec d = do 
    fix <- getFIXSpec d 
    let fields = filter isFields (content fix) in
        case fields of 
            (CElem es _):_ -> return es
            _ -> Nothing
    where
        isFields (CElem (Elem (N n) _ _) _ ) = n == "fields"
        isFields _ = False

fromAttributes :: LT.LookupTable String String AttrLookupTable 
                     => [Attribute] -> AttrLookupTable
fromAttributes = foldr _insert LT.new 
    where 
        _insert :: Attribute -> AttrLookupTable -> AttrLookupTable
        _insert (N k, AttValue ((Left v):_)) = LT.insert k v 
        _insert (N k, _) = LT.insert k ""

toParser :: String -> String
toParser x = fromMaybe "toFIXString" (LT.lookup x parserLT)
    where
        parserLT :: ParserLookupTable
        parserLT = LT.insert "INT" "toFIXInt" $
                   LT.insert "STRING" "toFIXString" $
                   LT.insert "DAYOFMONTH" "toFIXDayOfMonth" $
                   LT.insert "FLOAT" "toFIXFloat" $
                   LT.insert "QUANTITY" "toFIXQuantity" $
                   LT.insert "PRICE" "toFIXPrice" $
                   LT.insert "PRICEOFFSET" "toFIXPriceOffset" $
                   LT.insert "AMT" "toFIXAmt" $
                   LT.insert "BOOL" "toFIXBool" $
                   LT.insert "MULTIPLEVALUESTRING" "toFIXMultipleValueString" $
                   LT.insert "CURRENCY" "toFIXCurrency" $
                   LT.insert "EXCHANGE" "toFIXExchange" $
                   LT.insert "UTCTIMESTAMP" "toFIXUTCTimestamp" $
                   LT.insert "UTCTIMEONLY" "toFIXUTCTimeOnly" $
                   LT.insert "LOCALMKTDATE" "toFIXLocalMktDate" $
                   LT.new

genField :: Content a -> String
genField (CElem e _) = 
    let Just fname = LT.lookup "name" aLookup
        Just fenum = LT.lookup "number" aLookup            
        Just fparser = LT.lookup "type" aLookup
        tparser = toParser fparser
        in
        "t" ++ fname ++ " = FIXTag { tnum = " ++ fenum ++ 
            ", tparser = "  ++ tparser ++ " }\n"
    where
        aLookup = fromAttributes $ attributes e
genField _ = ""

xmlFile :: [String] -> String
xmlFile xs = xs !! 0

main = do
    args <- getArgs
    xmlContent <- readFile $ xmlFile args
    let xmlDoc = xmlParse "/dev/null" xmlContent
        Just fields = getFieldSpec xmlDoc in 
        putStr (concat $ map genField (content fields))
