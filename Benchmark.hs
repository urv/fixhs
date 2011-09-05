{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding ( foldr )
import qualified Prelude as P ( foldr )
import Criterion.Main
import Criterion.Config
import Data.FIX.FIX42
import Common.FIXParser
import Common.FIXCoparser
import Common.FIXMessage
import Data.Attoparsec hiding (take)
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C
import qualified Data.LookupTable as LT
import Test.QuickCheck.Gen 
import Control.Applicative ( (<$>) )
import Control.DeepSeq
import Data.Coparser (TextLike (..))
import Data.Text.Lazy.Builder ( Builder )

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
        parsingB (m, input) = let input' = coparse input :: Builder in 
            bench (msName m ++ " parsing") $ 
                nf (parseOnly (nm spec)) (C.pack $ unpack input')
        coparsingB (m, input) = bench (msName m ++ " coparsing") $ 
                nf (coparse :: FIXMessage FIXSpec -> Builder ) input

        bench1 = map coparsingB $ zip ms ss
        bench2 = map parsingB $ zip ms ss
    in
        ziczac bench1 bench2

    where
        ziczac :: [a] -> [a] -> [a]
        ziczac l r = P.foldr _construct [] $ zip l r
            where
                _construct :: (a, a) -> [a] -> [a]
                _construct (l, r) as = l : r : as

main = do 
        ss <- samples fix42
        defaultMainWith myConfig (return ()) [ bgroup "FIX42" $ benchmark fix42 ss ]
