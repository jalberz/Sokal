{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-04-15
-}


module Spew (executeSpew) where

--Imports
import Distribution.Simple
import Data.Array
import System.Random
import Control.Monad.State.Lazy

--Types
type FastModel = Array Int (String,[(Int,Int)])
type RandState = State StdGen


{-
function to read sokal.model, run randomization
models, and then spit out the resulting Sokal-mixed
string.
-}
executeSpew :: Int -> IO ()
executeSpew size = do
  model <- readFile "sokal.model"
  let readModel = map read (lines model) :: [(String, [(Int,Int)])]
      len = (length readModel) - 1
  gen <- getStdGen
  let markov = evalState (runSokalModel readModel size len) gen
  putStr $ linefill 72 markov

{-
My version on the random selection builds on the code from
CMSC 161 Lecture 18. An initial index is randomly generated
from the array and used to initiate an iterator that selects
successor words to add to the list of words.
-}
runSokalModel :: [(String, [(Int,Int)])] -> Int -> Int -> RandState [String]
runSokalModel proc size len = do
  index <- state $ randomR (bounds model)
  let start = model ! index
  iter start 0 where
    model = listArray (0, len) proc
    iter place tally = do
      successor <- sokalSelect place size tally
      case successor of
        Nothing -> return $ (fst place) : []
        Just i -> do
          remainder <- iter (model ! i) (tally + 1)
          return $ ((fst place): remainder)


{-
The weighted random select function. My version on the random selection 
builds on the select function from CMSC 161 Lecture 18. This function
first checks if the selection should continue or not depending on how
whether or not the requested input size has been reached or not (and
the end of a sentence has been reached). If not, then a new random index
is generated and passed to selectWithWeight
-}
sokalSelect :: (String, [(Int,Int)]) -> Int -> Int -> RandState (Maybe Int)
sokalSelect place tally size = 
  if (tally < size && (last $ fst place) /= '.')
    then do
      let 
        successors = snd place
        upper = sum ((map fst) successors)
      i <- state $ randomR (0,upper)
      return $ selectWithWeight i successors 
    else return $ Nothing

{-
A helper for the select function. This function goes through the
successors of a function (if they exist), if not empty, the randomly
generated int random is compared with the weight of a particular successor.
This random becomes more and more favorable to selection of a particular
successor as it walks down the list
-}
selectWithWeight :: Int -> [(Int, Int)] -> Maybe Int
selectWithWeight _ [] = Nothing
selectWithWeight random ((weight, ident):ls) 
  | (random - weight) > 0 = selectWithWeight (random - weight) ls
  | otherwise = Just ident


--NOTE: this code for preventing neverending text lines, has been copied from
--CMSC 161 Lecture 18
linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
  iter x [] = x ++ "\n"
  iter x (y:ys)
    | length x + length y + 1 > n = x ++ "\n" ++ linefill n (y:ys)
    | otherwise = iter (x ++ " " ++ y) ys

