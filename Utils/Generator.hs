{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

import Text.XML.HaXml
import qualified Text.XML.HaXml.Pretty as P 
import System.Environment ( getArgs )
import Data.LookupTable ( LookupTable )
import qualified Data.LookupTable as LT
import Data.Map ( Map )
import Data.Maybe ( fromMaybe )
import Control.Monad ( liftM )
import Control.Applicative ( (<$>) )


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
    name ++ " = \n" ++ tags' ++ "\n\n"
    where   
        tags' = let h = getHeaderSpec doc 
                in fieldsOf 6 $ content h

genFIXTrailer :: Document a -> String
genFIXTrailer doc = let name = trailerName doc in 
    name ++ " :: FIXTags\n" ++
    name ++ " = \n" ++ tags' ++ "\n\n"
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
                indent ++ "      " ++ msgBody' ++ " = \n" ++ tags' ++ "\n\n"

        getMsgTypeAttr = getAttr "msgtype"

genFIXFields :: Document a -> String
genFIXFields doc = concatMap fieldDef $ content $ getFieldSpec doc

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

type Groups a = Map String [Content a]

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

getSpec :: String -> Document a -> Maybe (Element a)
getSpec name doc = 
    let fix = getFIXSpec doc in
        case filter (_matchName name) (content fix)  of 
            CElem es _ : _ -> Just es
            _ -> Nothing

getFieldSpec = fromMaybe (error "fields not defined") . getSpec "fields"
getHeaderSpec = fromMaybe (error "header not defined") . getSpec "header"
getTrailerSpec = fromMaybe (error "trailer not defined") . getSpec "trailer"
getMessagesSpec = fromMaybe (error "messages not defined") . getSpec "messages"
getComponentsSpec = fromMaybe (Elem (N "components") [] []) . getSpec "components"

getAttr :: String -> Element a -> String
getAttr name e = _lookup name $ fromAttributes $ attributes e 

getNameAttr = getAttr "name"

fromAttributes :: [Attribute] -> Map String String 
fromAttributes = foldr _insert LT.new 
    where 
        _insert :: Attribute -> Map String String -> Map String String
        _insert (N k, AttValue (Left v : _)) = LT.insert k v 
        _insert (N k, _) = LT.insert k ""



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

type Components a = [(String, [Content a])]

componentsOf :: Document a -> Components a
componentsOf doc = let spec' = getComponentsSpec doc in
    foldr _insert LT.new $ content spec'
        where
            _insert c t 
                | _matchName "component" c = let elem = cElement c in
                          LT.insert (getNameAttr elem) (content elem) t
                | otherwise = t

expandComp :: Components a -> Content a -> Content a
expandComp comps c@(CElem (Elem n as cs) i) = 
    CElem (Elem n as (concatMap _expand cs)) i
    where
        _expand c | _matchName "component" c = 
                    let name = getNameAttr $ cElement c
                        compCont = _lookup name comps 
                    in
                        concatMap _expand compCont
                  | otherwise = [ expandComp comps c ]
expandComp comps c = c

messagesOf :: Document a -> [Content a]
messagesOf doc = 
    let all = getMessagesSpec doc 
        msgOnly = filter (_matchName "message") (content all) 
    in
        map (expandComp (componentsOf doc)) msgOnly

fieldsOf :: Int -> [Content a] -> String
fieldsOf i cs = 
    let groups = LT.toList $ groupsOf cs 
    in 
        concatMap _insertTag cs ++ 
        indent ++ "LT.new\n" ++
            if not (null groups) then
               indent ++ "where\n" ++ concatMap (genGroups (i+6)) groups
            else ""
                
    where 
        indent = replicate i ' '
        _insertTag c
            | _matchName "field" c = 
                    let n' = tName $ getNameAttr $ cElement c
                    in indent ++ "LT.insert (tnum " ++ n' ++ ") " ++ n' ++ " $\n"
            | _matchName "group" c = 
                    let n = getNameAttr $ cElement c
                    in indent ++ "LT.insert (tnum " ++ tName n ++ ") " ++ gName n ++ " $\n"
            | otherwise = ""
        
        genGroups _ (_, _:[]) = error "group should have a sperator"
        genGroups i (n, gcs') = 
            let (s, gcs) = getSepAndCont gcs'
                sname = tName $ getNameAttr $ cElement s 
                gname = gName n
                indent' = replicate i ' '
                tags' = fieldsOf (i + 8) gcs
            in
                indent' ++ gname ++ " = FIXTag\n" ++
                indent' ++ "  { tnum = tnum " ++ tName n ++ "\n" ++
                indent' ++ "  , tparser = " ++ gname ++ "P }\n\n" ++
                indent' ++ gname ++ "P = groupP $ FGSpec\n" ++
                indent' ++ "  { gsLength = " ++ tName n ++ "\n" ++
                indent' ++ "  , gsSeperator = " ++ sname ++ "\n" ++
                indent' ++ "  , gsBody = " ++ gname ++ "Body }\n" ++
                indent' ++ "    where\n" ++
                indent' ++ "    " ++ gname ++ "Body = \n"  ++ tags' ++ "\n"

            where
                getSepAndCont [] = undefined
                getSepAndCont (c@(CElem _ _):ds) = (c, ds)
                getSepAndCont (_ : ds) = getSepAndCont ds

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
                                in LT.insert gname gcontent gs'
                          | _matchName "message" c = 
                                let mcontent = content e in 
                                    addGroups gs mcontent 
                          | otherwise = gs
                _insert _ gs = gs 



