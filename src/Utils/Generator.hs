{-# LANGUAGE 
    GeneralizedNewtypeDeriving
  , FlexibleContexts #-}

import Text.XML.HaXml
import qualified Text.XML.HaXml.Pretty as P 
import System.Environment ( getArgs )
import Data.LookupTable ( LookupTable )
import qualified Data.LookupTable as LT
import Data.Map ( Map )
import Data.Maybe ( fromMaybe )
import Control.Monad ( liftM )
import Control.Applicative ( (<$>) )
import Data.FIX.Parser ( tBeginString, tBodyLength, tCheckSum, tMsgType )
import qualified Data.FIX.Message as FM ( tName ) 


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
            "module " ++ mod' ++ " ( " ++ fixSpecName doc ++ " ) where\n" ++
            "import qualified Data.ByteString.Char8 as C\n" ++ 
            "import qualified Data.LookupTable as LT ( new, insert )\n" ++
            "import Data.FIX.Message\n" ++ 
            "import Data.FIX.Parser\n" ++
            "import Data.Functor ( (<$>) )\n" ++
            "import Data.FIX.Arbitrary \n" ++
            "import Test.QuickCheck ( arbitrary )\n" 

        -- command line options 
        xmlFile :: [String] -> String
        xmlFile = head 

        moduleName :: [String] -> String
        moduleName xs | length xs > 1 = head $ tail xs
                      | otherwise = error 
                            "you need to specify a name for the module"

_commonTag :: Content a -> Bool
_commonTag (CElem e _) = 
               let tags = map FM.tName [tBeginString, tBodyLength, tCheckSum, tMsgType]
                   tag = getNameAttr e
               in 
                   elem tag tags
_commonTag _ = False

genFIXHeader :: Document a -> String
genFIXHeader doc = let name = headerName doc in
    name ++ " :: FIXTags\n" ++
    name ++ " = \n" ++ tags' ++ "\n\n"
    where   
        tags' = let h = getHeaderSpec doc 
                    header = filter (not . _commonTag) $ content h 
                in 
                    fieldsOf 1 header

genFIXTrailer :: Document a -> String
genFIXTrailer doc = let name = trailerName doc in 
    name ++ " :: FIXTags\n" ++
    name ++ " = \n" ++ tags' ++ "\n\n"
    where   
        tags' = let h = getTrailerSpec doc 
                    trailer = filter (not . _commonTag) $ content h 
                in 
                    fieldsOf 1 trailer

genFIXMessages :: Document a -> String
genFIXMessages doc = concatMap genMessage $ messagesOf doc
    where
        genMessage :: Content a -> String
        genMessage (CElem e _) = 
            let msg' = getNameAttr e
                msg = mName msg'
                msgBody' = msg ++ "Body"
                mType = getMsgTypeAttr e 
                tags' = fieldsOf 2 $ content e
                indent = replicate 3 ' '
            in 
                msg ++ " :: FIXMessageSpec\n" ++
                msg ++ " = FMSpec\n" ++
                indent ++ "{ msName = \"" ++ msg' ++ "\"\n" ++
                indent ++ ", msType = C.pack \"" ++ mType ++ "\"\n" ++
                indent ++ ", msHeader = " ++ headerName doc ++ '\n' :
                indent ++ ", msBody = " ++ msgBody' ++  '\n' :
                indent ++ ", msTrailer = " ++ trailerName doc ++ " }\n" ++
                indent ++ "where\n" ++
                indent ++ msgBody' ++ " = \n" ++ tags' ++ "\n\n"

        getMsgTypeAttr = getAttr "msgtype"

genFIXFields :: Document a -> String
genFIXFields doc = let fields = filter (not . _commonTag) $ 
                                    content $ getFieldSpec doc 
                    in concatMap fieldDef fields

genFIXSpec :: Document a -> String
genFIXSpec doc = let 
                spec' = fixSpecName doc 
                fix = getFIXSpec doc
                major = getAttr "major" fix
                minor = getAttr "minor" fix

               in  
                   spec' ++ " :: FIXSpec\n" ++
                   spec' ++ " = FSpec\n" ++
                   "   { fsVersion = \"FIX." ++ 
                        major ++ "." ++ minor++ "\"\n" ++ 
                   "   , fsHeader = " ++ headerName doc ++ '\n' :
                   "   , fsTrailer = " ++ trailerName doc ++ '\n' : 
                   "   , fsMessages = " ++ spec' ++ "Messages \n" ++
		   "   , fsTags = " ++ spec' ++ "Tags }\n" ++
                   "   where\n" ++
                   "      " ++ spec' ++ "Messages =\n" ++ messageMap ++
                   "          LT.new \n" ++
		   "      " ++ spec' ++ "Tags =\n" ++ tagsMap ++
                   "          LT.new \n" 
               where
                rmLastNewline text | length text > 2 = init . init $ text
                                   | otherwise = text
                messageMap = rmLastNewline (concatMap _insertMsg $ messagesOf doc)
                _insertMsg (CElem e _) = 
                    let msg' = mName $ getNameAttr e in 
                        "          LT.insert (msType " ++ msg' ++ ") " ++ 
                        msg' ++ " $\n" 
                _insertMsg _ = undefined
		tagsMap = concatMap _insertTag $ content $ getFieldSpec doc 
		_insertTag (CElem e _) = let fname = tName (getNameAttr e) in
                        "          LT.insert (tnum " ++ fname ++ ") " ++ fname ++ " $\n"
		_insertTag _ = ""

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

_lookup :: (LookupTable t, Show (LT.KeyOf t)) => LT.KeyOf t -> t -> LT.ValueOf t
_lookup key t = fromMaybe (error $ "couldn't find key " ++ show key)
                    $ LT.lookup key t

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
    let name = getNameAttr e
        fname = tName name
        fenum = getAttr "number" e
        ftype = getAttr "type" e
        tparser = "to" ++ typeOfFIX ftype
	arbValue' = let t' = typeOfFIX ftype in 
		case t' of 
		"FIXChar" -> "FIXChar <$> (return 'A')"
		"FIXDouble" -> "FIXDouble <$> (return (-2.112 :: Double))"
		_ -> t' ++ " <$> arbitrary" 
    in
        fname ++ " :: FIXTag\n" ++ 
        fname ++ " = FIXTag \n" ++ 
            "   { tName = \"" ++ name ++ "\"\n" ++
            "   , tnum = " ++ fenum ++ '\n' :
            "   , tparser = "  ++ tparser ++ '\n' :
            "   , arbitraryValue = " ++ arbValue' ++ " }\n\n"
    where

        typeOfFIX :: String -> String
        typeOfFIX x = fromMaybe (error $ "unknown type " ++ x) $ 
                        LT.lookup x values' 
            where
                values' :: Map String String
                values' = 
                    LT.insert "INT" "FIXInt" $
                    LT.insert "STRING" "FIXString" $
                    LT.insert "DAYOFMONTH" "FIXInt" $
                    LT.insert "CHAR" "FIXChar" $
                    LT.insert "FLOAT" "FIXDouble" $
                    LT.insert "QTY" "FIXDouble" $
                    LT.insert "PRICE" "FIXDouble" $
                    LT.insert "QUANTITY" "FIXDouble" $
                    LT.insert "PRICEOFFSET" "FIXDouble" $
                    LT.insert "AMT" "FIXDouble" $
                    LT.insert "BOOLEAN" "FIXBool" $
                    LT.insert "MULTIPLEVALUESTRING" "FIXMultipleValueString" $
                    LT.insert "CURRENCY" "FIXString" $
                    LT.insert "EXCHANGE" "FIXString" $
                    LT.insert "UTCTIMESTAMP" "FIXTimestamp" $
                    LT.insert "UTCTIMEONLY" "FIXTimeOnly" $
                    LT.insert "UTCDATE" "FIXDateOnly" $
                    LT.insert "MONTHYEAR" "FIXMonthYear" $
                    LT.insert "LOCALMKTDATE" "FIXDateOnly" $
                    LT.insert "DATA" "FIXData" $
                    LT.insert "LENGTH" "FIXInt" $
                    LT.insert "TIME" "FIXTimestamp" $ 
                    LT.insert "SEQNUM" "FIXInt"  $
                    LT.insert "NUMINGROUP" "FIXInt" $
                    LT.insert "PERCENTAGE" "FIXDouble" $
                    LT.insert "COUNTRY" "FIXString" $ 
                    LT.insert "UTCDATEONLY" "FIXDateOnly" $
                    LT.insert "DATE" "FIXDateOnly" 
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
fieldsOf l cs = 
    let groups = LT.toList $ groupsOf cs 
        rmLastNewline text | length text > 2 = init . init $ text
                           | otherwise = text
    in 
        rmLastNewline (concatMap _insertTag cs) ++ 
        indent ++ "LT.new\n" ++
            if not (null groups) then
               indent ++ "where\n" ++ concatMap (genGroups (l + 1)) groups
            else ""
                
    where 
        indent = replicate (l * 3)  ' '
        suffix = replicate (l + 1) '\''
        _insertTag c
            | _matchName "field" c = 
                    let n' = tName $ getNameAttr $ cElement c
                    in indent ++ "LT.insert (tnum " ++ n' ++ ") " ++ n' ++ " $\n"
            | _matchName "group" c = 
                    let n = getNameAttr $ cElement c
                    in indent ++ "LT.insert (tnum " ++ tName n ++ ") " ++ gName n ++ suffix ++ " $\n"
            | otherwise = ""
        
        genGroups _ (_, _:[]) = error "group should have a sperator"
        genGroups l (n, gcs') = 
            let (s, gcs) = getSepAndCont gcs'
                sname = tName $ getNameAttr $ cElement s 
                gname = gName n
                indent' = replicate (l * 3)  ' '
                indent'' = replicate ((l + 1) * 3)  ' '
                suffix' = replicate l '\''
                tags' = fieldsOf (l + 2) gcs
            in
                indent' ++ gname ++ suffix' ++ " = FIXTag\n" ++
                indent'' ++ "{ tName = \"" ++ n ++ "\"\n" ++
                indent'' ++ ", tnum = tnum " ++ tName n ++ '\n' :
                indent'' ++ ", tparser = " ++ gname ++ "P" ++ suffix' ++ '\n' : 
                indent'' ++ ", arbitraryValue = arbibtraryFIXGroup " ++ 
                                gname ++ "Spec" ++ suffix' ++ " }\n\n" ++
                indent' ++ gname ++ "P" ++ suffix' ++ " = groupP " ++ gname ++ "Spec" ++ suffix' ++ '\n' :
                indent' ++ gname ++ "Spec" ++ suffix' ++ " = FGSpec\n" ++
                indent'' ++ "{ gsLength = " ++ tName n ++ '\n' :
                indent'' ++ ", gsSeperator = " ++ sname ++ '\n' :
                indent'' ++ ", gsBody = " ++ gname ++ "Body" ++ suffix' ++ " }\n" ++
                indent'' ++ "where\n" ++
                indent'' ++ gname ++ "Body" ++ suffix' ++ " = \n"  ++ tags' ++ "\n"

            where
                getSepAndCont [] = undefined
                getSepAndCont (c@(CElem _ _):ds) = (c, ds)
                getSepAndCont (_ : ds) = getSepAndCont ds

groupsOf :: [Content a] -> Groups a
groupsOf = addGroups LT.new 
    where
        addGroups :: Groups a -> [Content a] -> Groups a
        addGroups = foldr _insert 
            where 
                _insert :: Content a -> Groups a -> Groups a
                _insert c@(CElem e _) gs
                          | _matchName "group" c = 
                                let gname = getNameAttr e
                                    gcontent = content e
                                in LT.insert gname gcontent gs
                          | _matchName "message" c = 
                                let mcontent = content e in 
                                    addGroups gs mcontent 
                          | otherwise = gs
                _insert _ gs = gs 



