{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-04-15
-}

module Model (makeModel) where

import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

type PrimitiveModel = Map.Map (String,String) [String]

type ProcessedModel = [(String,[(Int,Int)])]

makeModel :: [String] -> ProcessedModel
makeModel ls = makeProcessed $ frequencies $ makePrimitive ls


--Turning a list of strings to a primitive model
makePrimitive :: [String] -> PrimitiveModel
makePrimitive l =
	let
		masterlist = concatMap (\s -> zip (firstwords s) (followingwords s)) (map words l)
	in
		Map.fromListWith (++) masterlist where
			followingwords list = map (\l -> [l]) (tail $ tail list)
			firstwords list = zip list (tail list)

--A function for the purposes of calculating the frequency of words
frequencies :: PrimitiveModel -> [((String,String), [(Int, String)])]
frequencies prim = acqFrequency prim (Map.keys prim) 

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

--Finishing function to create a fully processed model
makeProcessed :: [((String,String), [(Int,String)])] -> ProcessedModel
makeProcessed strs = map (\((x,y), fs) -> (y, fs)) index where
	index = map (\((x,y),fs) -> ((x,y), searchNUpdate fs y)) strs
	searchNUpdate freqs mark = 
		case freqs of
			[] -> []
			(count,word):fs -> 
				case findIndex (verifyCopy (mark, word)) strs of
					Nothing -> searchNUpdate fs mark
					Just ix -> (count,ix):searchNUpdate fs mark
	verifyCopy copy (x,_) = copy == x
	
