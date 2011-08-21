import Text.XML.HaXml
import System.Environment ( getArgs )


content :: Element a -> [Content a]
content (Elem _ _ cs) = cs

attributes :: Element a -> [Attribute]
attributes (Elem _ as _) = as

getFIXSpec :: Document a -> Maybe (Element a)
getFIXSpec d = case d of
    Document _ _ (es@(Elem (n) _ _)) _ 
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
        isFields (CElem (Elem n _ _) _ ) = n == "fields"
        isFields _ = False

xmlFile :: [String] -> String
xmlFile xs = xs !! 0

main = do
    args <- getArgs
    xmlContent <- readFile $ xmlFile args
    let Just fields = getFieldSpec (xmlParse "/dev/null" xmlContent)
        in print $ length (content fields)
