{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

import Text.XML.HaXml
import qualified Text.XML.HaXml.Pretty as P 
import System.Environment ( getArgs )
import Data.LookupTable ( LookupTable )
import qualified Data.LookupTable as LT
import Data.Map ( Map )
import Data.Maybe ( fromMaybe )
import Control.Monad ( liftM )


main = do
    args <- getArgs
    xmlContent <- readFile $ xmlFile args
    let xmlDoc = xmlParse "/dev/null" xmlContent
        modName = moduleName args in 
        do --- print the module header with all the imports ---  
           putStr $ moduleHeader modName xmlDoc ++ "\n\n"
           {-putStr $ concatMap (show . P.content) $ content $ getFieldSpec xmlDoc-}

           --- declare all FIX Tags ---
           putStr $ genFIXFields xmlDoc

           --- declare the FIX header ---
           putStr $ genFIXHeader xmlDoc

           --- declare the FIX trailer ---
           putStr $ genFIXTrailer xmlDoc

           --- declare all FIX messages ---
           putStr $ genFIXMessages xmlDoc

           {-putStr $ genFIXGroups xmlDoc-}
           putStr $ genFIXSpec xmlDoc
    where
        moduleHeader mod' doc = 
            "module " ++ mod' ++ " where\n" ++
            "import qualified Data.ByteString.Char8 as C\n" ++ 
            "import qualified Data.LookupTable as LT ( new, insert )\n" ++
            "import Common.FIXMessage\n" ++ 
            "import Common.FIXParser\n" 

        -- command line options 
        xmlFile :: [String] -> String
        xmlFile = head 

        moduleName :: [String] -> String
        moduleName xs | length xs > 1 = head $ tail xs
                      | otherwise = error 
                            "you need to specify a name for the module"



genFIXHeader :: Document a -> String
genFIXHeader doc = let name = headerName doc in
    name ++ " :: FIXTags\n" ++
    name ++ " = \n" ++ tags' ++ "    LT.new\n\n"
    where   
        tags' = let h = getHeaderSpec doc 
                in fieldsOf 6 $ content h

genFIXTrailer :: Document a -> String
genFIXTrailer doc = let name = trailerName doc in 
    name ++ " :: FIXTags\n" ++
    name ++ " = \n" ++ tags' ++ "    LT.new\n\n"
    where   
        tags' = let h = getTrailerSpec doc 
                in fieldsOf 6 $ content h

genFIXMessages :: Document a -> String
genFIXMessages doc = concatMap (genMessage 0) $ messagesOf doc
    where
        genMessage :: Int -> Content a -> String
        genMessage i (CElem e _) = 
            let msg' = mName $ getNameAttr e
                msgBody' = msg' ++ "Body"
                mType = getMsgTypeAttr e 
                tags' = fieldsOf (i + 10) $ content e
                indent = replicate i ' '
            in 
                msg' ++ " :: FIXMessageSpec\n" ++
                msg' ++ " = FMSpec\n" ++
                indent ++ "   { msType = C.pack \"" ++ mType ++ "\"\n" ++
                indent ++ "   , msHeader = " ++ headerName doc ++ "\n" ++
                indent ++ "   , msBody = " ++ msgBody' ++  "\n" ++
                indent ++ "   , msTrailer = " ++ trailerName doc ++ " }\n" ++
                indent ++ "   where\n" ++
                indent ++ "      " ++ msgBody' ++ " = \n" ++ tags' ++ 
                indent ++ "          LT.new\n\n"

        getMsgTypeAttr = getAttr "msgtype"


genFIXSpec :: Document a -> String
genFIXSpec doc = let spec' = fixSpecName doc 
               in  
                   spec' ++ " :: FIXSpec\n" ++
                   spec' ++ " = FSpec\n" ++
                   "   { fsHeader = " ++ headerName doc ++ "\n" ++
                   "   , fsTrailer = " ++ trailerName doc ++ "\n" ++
                   "   , fsMessages = " ++ spec' ++ "Messages }\n" ++
                   "   where\n" ++
                   "      " ++ spec' ++ "Messages =\n" ++
                       messageMap ++
                   "          LT.new \n"
               where
                messageMap = concatMap _insertMsg $ messagesOf doc
                _insertMsg (CElem e _) = 
                    let msg' = mName $ getNameAttr e in 
                        "          LT.insert (msType " ++ msg' ++ ") " ++ 
                        msg' ++ " $\n" 
                _insertMsg _ = undefined

newtype Groups a = G (Map String Bool)
    deriving (LT.LookupTable String Bool)

-- ?Name :: String -> String 
tName = (:) 't'
mName = (:) 'm'
fName = (:) 'f'
gName = (:) 'g'

versionFIX :: Document a -> String
versionFIX doc = major ++ minor 
               where 
                  fix = getFIXSpec doc 
                  major = getAttr "major" fix
                  minor = getAttr "minor" fix

-- *Name :: Document a -> String 
headerName  d = "headerFIX" ++ versionFIX d
trailerName d = "trailerFIX" ++ versionFIX d
fixSpecName a = "fix" ++ versionFIX a

content :: Element a -> [Content a]
content (Elem _ _ cs) = cs

cElement :: Content a -> Element a
cElement (CElem e _) = e
cElement _ = error "not an element"

attributes :: Element a -> [Attribute]
attributes (Elem _ as _) = as

getFIXSpec :: Document a -> Element a
getFIXSpec doc = case doc of
    Document _ _ (es@(Elem (N n) _ _)) _ 
        -> if n == "fix" then es else error "no specification for fix" 
    _   -> error "unknown error"


_matchName :: String -> Content a -> Bool
_matchName name (CElem (Elem (N n) _ _) _) = n == name
_matchName name _ = False

_lookup :: LookupTable k v t => k -> t -> v
_lookup key t = fromMaybe undefined $ LT.lookup key t

getSpec :: String -> Document a -> Element a
getSpec name doc = 
    let fix = getFIXSpec doc 
        specs = filter (_matchName name) (content fix) 
    in
        case specs of 
            CElem es _ : _ -> es
            _ -> error $ "no specification for " ++ name

getFieldSpec = getSpec "fields"
getHeaderSpec = getSpec "header"
getTrailerSpec = getSpec "trailer"
getMessagesSpec = getSpec "messages"

getAttr :: String -> Element a -> String
getAttr name e = _lookup name $ fromAttributes $ attributes e 

getNameAttr = getAttr "name"

fromAttributes :: [Attribute] -> Map String String 
fromAttributes = foldr _insert LT.new 
    where 
        _insert :: Attribute -> Map String String -> Map String String
        _insert (N k, AttValue (Left v : _)) = LT.insert k v 
        _insert (N k, _) = LT.insert k ""

genFIXFields :: Document a -> String
genFIXFields doc = let 
                     groups = groupsOf $ messagesOf doc
                     fields = let flds' = content $ getFieldSpec doc 
                                  flds = filter isElement flds'
                              in 
                                  filter (not . inGroup) flds
                        where
                            isElement (CElem _ _) = True
                            isElement _ = False

                            inGroup = inGroup' . getNameAttr . cElement
                            inGroup' n = fromMaybe False (LT.lookup n groups)
                   in 
                     concatMap fieldDef fields


fieldDef :: Content a -> String
fieldDef (CElem e _) = 
    let fname = tName $ _lookup "name" attributeTable
        fenum = _lookup "number" attributeTable            
        ftype = _lookup "type" attributeTable
        tparser = toParser ftype
    in
        fname ++ " :: FIXTag\n" ++ 
        fname ++ " = FIXTag { tnum = " ++ fenum ++ 
            ", tparser = "  ++ tparser ++ " }\n\n"
    where
        attributeTable = fromAttributes $ attributes e

        toParser :: String -> String
        toParser x = fromMaybe "toFIXString" (LT.lookup x valParsers) 
            where
                valParsers :: Map String String
                valParsers = 
                    LT.insert "INT" "toFIXInt" $
                    LT.insert "STRING" "toFIXString" $
                    LT.insert "DAYOFMONTH" "toFIXDayOfMonth" $
                    LT.insert "CHAR" "toFIXChar" $
                    LT.insert "FLOAT" "toFIXFloat" $
                    LT.insert "QTY" "toFIXQuantity" $
                    LT.insert "PRICE" "toFIXPrice" $
                    LT.insert "PRICEOFFSET" "toFIXPriceOffset" $
                    LT.insert "AMT" "toFIXAmt" $
                    LT.insert "BOOLEAN" "toFIXBool" $
                    LT.insert "MULTIPLEVALUESTRING" 
                              "toFIXMultipleValueString" $
                    LT.insert "CURRENCY" "toFIXCurrency" $
                    LT.insert "EXCHANGE" "toFIXExchange" $
                    LT.insert "UTCTIMESTAMP" "toFIXUTCTimestamp" $
                    LT.insert "UTCTIMEONLY" "toFIXUTCTimeOnly" $
                    LT.insert "LOCALMKTDATE" "toFIXLocalMktDate" 
                    LT.new
fieldDef _ = ""


messagesOf :: Document a -> [Content a]
messagesOf doc = let all = getMessagesSpec doc in 
                    filter (_matchName "message") (content all) 

fieldsOf :: Int -> [Content a] -> String
fieldsOf i = concatMap _insertTag 
    where 
        indent = replicate i ' '
        _insertTag c
            | _matchName "field" c = 
                    let n' = tName $ getNameAttr $ cElement c
                    in indent ++ "LT.insert (tnum " ++ n' ++ ")\n" ++ 
                       indent ++ "          " ++ n' ++ " $\n"
            | otherwise = ""

groupsOf :: [Content a] -> Groups a
groupsOf cs = addGroups LT.new cs
    where
        addGroups :: Groups a -> [Content a] -> Groups a
        addGroups t = foldr _insert t 
            where 
                _insert :: Content a -> Groups a -> Groups a
                _insert c@(CElem e _) gs
                          | _matchName "group" c = 
                                let gname = getNameAttr e
                                    gcontent = content e
                                    gs' = addGroups gs gcontent
                                in LT.insert gname True gs'
                          | _matchName "message" c = 
                                let mcontent = content e in 
                                    addGroups gs mcontent 
                          | otherwise = gs
                _insert _ gs = gs 



