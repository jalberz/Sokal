{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-04-15
-}

module MakeModel (makeModel) where

--import Distribution Simple
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

--Types
type PrimitiveModel = Map.Map (String,String) [String]
type ProcessedModel = [(String,[(Int,Int)])]

{-
Umbrella function for making processed models
-}
makeModel :: [String] -> [((String,String), [(Int, String)])]
makeModel ls = frequencies $ mkPrimitive ls

{-
Initial function to turn the list of articles into
a primitive model.
-}
mkPrimitive :: [String] -> PrimitiveModel
mkPrimitive strs = case strs of
        (a:b:c) -> Map.alter
                (checkCurrent)
                (a, b)
                (mkPrimitive (b:c)) where
                        checkCurrent Nothing = if (c == []) then Nothing else Just $ [head c]
                        checkCurrent (Just r) = if (c == []) then Nothing else Just $ ((head c):r)
        _ -> Map.empty

{-
A function for the purposes of calculating the frequency of words
-}
frequencies :: PrimitiveModel -> [((String,String), [(Int, String)])]
frequencies prim = acqFrequency prim (Map.keys prim) where
    --A helper function for the purposes of calculating frequency
    acqFrequency p (n:ns) =
            let
                txt = p Map.! n
                freqcount = accumulateFreq txt (nub txt)
            in
                (n, freqcount) : acqFrequency p ns where
                    accumulateFreq txt (n:ns) = accum : accumulateFreq txt ns where
                        total = elemIndices n txt
                        accum = (length total, n)
                    accumulateFreq _ [] = []
    acqFrequency p [] = []

{-
Finishing function to create a fully processed model
-}
mkProcessed :: [((String,String), [(Int,String)])] -> ProcessedModel
mkProcessed strs = map (\((x,y), fs) -> (y, fs)) index where
        index = map (\((x,y),fs) -> ((x,y), searchNUpdate fs y)) strs
        searchNUpdate freqs mark =
                case freqs of
                        [] -> []
                        (count,word):fs ->
                                case findIndex (verifyCopy (mark, word)) strs of
                                        Nothing -> searchNUpdate fs mark
                                        Just ix -> (count,ix):searchNUpdate fs mark
        verifyCopy copy (x,_) = copy == x

