{-# LANGUAGE FlexibleContexts #-}
import Text.XML.HaXml
import System.Environment ( getArgs )
import qualified Data.LookupTable as LT
import Data.Map ( Map )
import Data.Maybe ( fromMaybe )
import Control.Monad ( liftM )


tName :: String -> String
tName = (:) 't'

mName :: String -> String
mName = (:) 'm'

fName :: String -> String
fName = (:) 'f'

headerName :: Document a -> String
headerName d = "headerFIX" ++ versionFIX d

trailerName :: Document a -> String
trailerName d = "trailerFIX" ++ versionFIX d

fixSpecName :: Document a -> String
fixSpecName a = "fix" ++ versionFIX a

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

versionFIX :: Document a -> String
versionFIX d = major ++ minor 
               where 
                  fix = fromMaybe undefined $ getFIXSpec d 
                  major = fromMaybe undefined $ getAttr "major" fix
                  minor = fromMaybe undefined $ getAttr "minor" fix

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
getHeaderSpec = getSpec "header"
getTrailerSpec = getSpec "trailer"
getMessagesSpec = getSpec "messages"

getAttr :: String -> Element a -> Maybe String
getAttr name e = LT.lookup name $ fromAttributes (attributes e)

getNameAttr = getAttr "name"

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
    let fname = tName $ fromMaybe undefined (LT.lookup "name" aLookup)
        fenum = fromMaybe undefined $ LT.lookup "number" aLookup            
        fparser = fromMaybe undefined $ LT.lookup "type" aLookup
        tparser = toParser fparser
        in
        fname ++ " :: FIXTag\n" ++ 
        fname ++ " = FIXTag { tnum = " ++ fenum ++ 
            ", tparser = "  ++ tparser ++ " }\n\n"
    where
        aLookup = fromAttributes $ attributes e
genField _ = ""

getFields :: [Content a] -> [String]
getFields [] = []
getFields (CElem e@(Elem (N n) _ _) _ : cs) =
    let rest = getFields cs 
        eName = fromMaybe undefined (getNameAttr e)
    in
        (if n == "field" then (:) eName else id) rest
getFields (_ : cs) = getFields cs

allMessages :: Document a -> [Content a]
allMessages d = let all = fromMaybe undefined $ getMessagesSpec d 
                    in filter (_matchName "message") (content all) 

allTags :: [String] -> [String]
allTags fs = map _insertTag fs
    where 
        _insertTag n = let n' = tName n 
                       in "LT.insert (tnum " ++ n' ++ ") " ++ n' ++ " $\n"

genFIXHeader :: Document a -> String
genFIXHeader d = let name = headerName d in
    name ++ " :: FIXTags\n" ++
    name ++ " = \n" ++ concatMap ((++) "    ") tags' ++ "    LT.new\n\n"
    where   
        tags' = let Just h = getHeaderSpec d 
                in allTags $ getFields (content h)

genFIXTrailer :: Document a -> String
genFIXTrailer d = let name = trailerName d in 
    name ++ " :: FIXTags\n" ++
    name ++ " = \n" ++ concatMap ((++) "    ") tags' ++ "    LT.new\n\n"
    where   
        tags' = let Just h = getTrailerSpec d 
                in allTags $ getFields (content h)

genMessages :: Document a -> String
genMessages d = concatMap genMessage $ allMessages d
    where
        genMessage :: Content a -> String
        genMessage (CElem e _) = 
            let msg' = mName $ fromMaybe undefined (getNameAttr e)
                msgBody' = msg' ++ "Body"
                mType = fromMaybe undefined $ getMsgTypeAttr e 
                tags' = allTags $ getFields (content e) 
            in 
                msg' ++ " :: FIXMessageSpec\n" ++
                msg' ++ " = FMSpec\n" ++
                "   { mType = C.pack \"" ++ mType ++ "\"\n" ++
                "   , mHeader = " ++ headerName d ++ "\n" ++
                "   , mBody = " ++ msgBody' ++  "\n" ++
                "   , mTrailer = " ++ trailerName d ++ " }\n" ++
                "   where\n" ++
                "      " ++ msgBody' ++ " = \n" ++ 
                concatMap ((++) "          ") tags' ++ 
                "          LT.new\n\n"

        getMsgTypeAttr = getAttr "msgtype"


genFIXSpec :: Document a -> String
genFIXSpec d = let spec' = fixSpecName d 
               in  
                   spec' ++ " :: FIXSpec\n" ++
                   spec' ++ " = FSpec\n" ++
                   "   { fHeader = " ++ headerName d ++ "\n" ++
                   "   , fTrailer = " ++ trailerName d ++ "\n" ++
                   "   , fMessages = " ++ spec' ++ "Messages }\n" ++
                   "   where\n" ++
                   "      " ++ spec' ++ "Messages =\n" ++
                       messageMap ++
                   "          LT.new \n"
               where
                messageMap = concatMap _insertMsg $ allMessages d
                _insertMsg (CElem e _) = 
                    let msg' = mName $ fromMaybe undefined (getNameAttr e) in 
                        "          LT.insert (mType " ++ msg' ++ ") " ++ 
                        msg' ++ " $\n" 
                _insertMsg _ = undefined


xmlFile :: [String] -> String
xmlFile = head 

moduleName :: [String] -> String
moduleName xs | length xs > 1 = head $ tail xs
              | otherwise = "Dummy"

main = do
    args <- getArgs
    xmlContent <- readFile $ xmlFile args
    let xmlDoc = xmlParse "/dev/null" xmlContent
        modName = moduleName args
        fields = fromMaybe undefined $ getFieldSpec xmlDoc in 
        do putStr $ moduleHeader modName xmlDoc 
           putStr "\n\n"
           putStr $ concatMap genField (content fields)
           putStr $ genFIXHeader xmlDoc
           putStr $ genFIXTrailer xmlDoc
           putStr $ genMessages xmlDoc
           putStr $ genFIXSpec xmlDoc
    where
        moduleHeader mod' d = 
            "module " ++ mod' ++ " where\n" ++
            "import qualified Data.ByteString.Char8 as C\n" ++ 
            "import qualified Data.LookupTable as LT ( new, insert )\n" ++
            "import Common.FIXMessage\n" ++ 
            "import Common.FIXParser\n" 
