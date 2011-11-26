{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ( foldr )
import qualified Prelude as P ( foldr )
import Criterion.Main
import Criterion.Config
import Data.FIX.Spec.FIX42
import Data.FIX.Parser
import Data.FIX.Coparser
import Data.FIX.Message
import Data.Attoparsec hiding (take, parse)
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C
import qualified Data.LookupTable as LT
import Test.QuickCheck.Gen 
import Control.Applicative ( (<$>) )
import Control.DeepSeq
import Data.DList ( DList )
import Data.Coparser ( unpack )
import Data.FIX.Arbitrary
import Control.DeepSeq ( NFData (..) )

myConfig = defaultConfig 

samples :: FIXSpec -> IO [FIXMessage FIXSpec]
samples spec = 
    let ms = map snd $ LT.toList $ fsMessages spec in
        mapM genSample ms
    where
       genSample :: FIXMessageSpec -> IO (FIXMessage FIXSpec)
       genSample m' = let randMsg = arbitraryFIXMessage spec in
                          head <$> sample' (randMsg m')
                
benchmark :: FIXSpec -> [FIXMessage FIXSpec] -> [Benchmark]
benchmark spec ss = 
    let ms = map snd $ LT.toList $ fsMessages spec 
        parsingB (m, input) = let input' = coparse input :: ByteString in 
            bench (msName m ++ " parsing") $ 
                nf (parse (nextP >>= messageP fix42)) (C.pack $ unpack input')
        coparsingB (m, input) = bench (msName m ++ " coparsing") $ 
                nf (coparse :: FIXMessage FIXSpec -> ByteString ) input

        bench1 = map coparsingB $ zip ms ss
        bench2 = map parsingB $ zip ms ss
    in
        ziczac bench1 bench2

    where
    	parse :: Parser (FIXMessage FIXSpec) -> ByteString -> FIXMessage FIXSpec
        parse p b = case parseOnly p b of 
		Left err -> error err
		Right m -> m
        ziczac :: [a] -> [a] -> [a]
        ziczac l r = P.foldr _construct [] $ zip l r
            where
                _construct :: (a, a) -> [a] -> [a]
                _construct (l, r) as = l : r : as

main = do 
        ss <- samples fix42
        defaultMainWith myConfig (return ()) [ bgroup "FIX42" $ benchmark fix42 ss ]


instance Control.DeepSeq.NFData ByteString 
instance Control.DeepSeq.NFData CalendarTime
instance Control.DeepSeq.NFData FIXGroupElement where
    rnf (FIXGroupElement _ s vs) = rnf s `seq` rnf vs 

instance Control.DeepSeq.NFData FIXValue where
    rnf (FIXInt x) = rnf x
    rnf (FIXDouble x) = rnf x
    rnf (FIXChar x) = rnf x
    rnf (FIXBool x) = rnf x
    rnf (FIXString x) = rnf x
    rnf (FIXMultipleValueString x) = rnf x
    rnf (FIXTimestamp x) = rnf x
    rnf (FIXTimeOnly x) = rnf x
    rnf (FIXDateOnly x) = rnf x
    rnf (FIXMonthYear x) = rnf x
    rnf (FIXData x) = rnf x 
    rnf (FIXGroup l es) = rnf l `seq` rnf es

instance Control.DeepSeq.NFData (FIXMessage a) where
    rnf (FIXMessage _ _ h b t) = rnf h `seq` rnf b `seq` rnf t

instance NFData Builder where
    rnf = rnf . Builder.unpack . Builder.toLazyText

instance NFData (DL.DList Char) where
    rnf = rnf . unpack

instance NFData (ListOfValues a) where
    rnf (LoV m) = rnf $ LT.toList m
