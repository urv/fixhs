{-# LANGUAGE FlexibleContexts #-}
import Text.XML.HaXml
import System.Environment ( getArgs )
import qualified Data.LookupTable as LT
import Data.Map ( Map )
import Data.Maybe ( fromMaybe )
import Control.Monad ( liftM )

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


_matchName :: String -> Content a -> Bool
_matchName name (CElem (Elem (N n) _ _) _) = n == name
_matchName name _ = False

getSpec :: String -> Document a -> Maybe (Element a)
getSpec name d = do 
    fix <- getFIXSpec d 
    let specs = filter (_matchName name) (content fix) in
        case specs of 
            CElem es _ : _ -> return es
            _ -> Nothing

getFieldSpec = getSpec "fields"

getAttr :: String -> Element a -> Maybe String
getAttr name e = LT.lookup name $ fromAttributes (attributes e)


fromAttributes :: LT.LookupTable String String AttrLookupTable 
                     => [Attribute] -> AttrLookupTable
fromAttributes = foldr _insert LT.new 
    where 
        _insert :: Attribute -> AttrLookupTable -> AttrLookupTable
        _insert (N k, AttValue (Left v : _)) = LT.insert k v 
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
                   LT.insert "LOCALMKTDATE" "toFIXLocalMktDate" 
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


genMessages :: Document a -> String
genMessages d = concatMap genMessage messages
    where
        getHeaderSpec = getSpec "header"
        getTrailerSpec = getSpec "trailer"
        getMessagesSpec = getSpec "messages"

        messages = let all = fromMaybe undefined $ getMessagesSpec d in
                       filter (_matchName "message") (content all) 

        mHeader  = let Just h = getHeaderSpec d in getFields (content h)
        mTrailer = let Just h = getTrailerSpec d in getFields (content h)

        genMessage :: Content a -> String
        genMessage (CElem e _) = 
            let Just mName = getNameAttr e 
                Just mType = getMsgTypeAttr e 
                mExprName = 'm' : mName
                allFields = mHeader ++ getFields (content e) ++ mTrailer
                allTags px = concatMap _insertTag allFields
                    where 
                        _insertTag n = 
                            let tName = 't' : n in 
                                px ++ "LT.insert (tnum " ++ tName ++ ") " ++ tName ++ " $\n"
            in 
                mExprName ++ " :: FIXMessageSpec\n" ++
                mExprName ++ " = FMSpec { mType = (C.pack \"" ++ mType ++ "\"), mTags = "
                    ++ mExprName ++ "Tags }\n" ++
                "   where\n" ++
                "      " ++ mExprName ++ "Tags = \n" ++ 
                allTags "          " ++ 
                "          LT.new\n\n"

        getNameAttr = getAttr "name"
        getMsgTypeAttr = getAttr "msgtype"

        getFields :: [Content a] -> [String]
        getFields [] = []
        getFields (CElem e@(Elem (N n) _ _) _ : cs) =
            let rest = getFields cs 
                eName = fromMaybe undefined (getNameAttr e)
            in
                (if n == "field" then (:) eName else id) rest
        getFields (_ : cs) = getFields cs



xmlFile :: [String] -> String
xmlFile = head 

main = do
    args <- getArgs
    xmlContent <- readFile $ xmlFile args
    let xmlDoc = xmlParse "/dev/null" xmlContent
        Just fields = getFieldSpec xmlDoc in 
        do putStr (concatMap genField (content fields))
           putStr $ genMessages xmlDoc
