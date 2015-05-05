{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-04-15
-}


module Spew (executeSpew) where

import Distribution.Simple
import Data.Array
import System.Random
import Control.Monad.State.Lazy

type FastModel = Array Int (String,[(Int,Int)])

type RandState = State StdGen

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
CMSC 161 Lecture 18
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
My version on the random selection builds on the code from
CMSC 161 Lecture 18
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


selectWithWeight :: Int -> [(Int, Int)] -> Maybe Int
selectWithWeight _ [] = Nothing
selectWithWeight i ((weight, ident):ls) 
  | (i - weight) > 0 = selectWithWeight (i - weight) ls
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

{-}
-- |This is heavily inspired (read: copied verbatim then modified) from Lecture 18 from 16100.
markovSelect :: [(Int,Int)] -> RandState Int
markovSelect successors = fmap (weightedSelect successors) . state . randomR $ (0,sum $ (map fst) successors) where
  weightedSelect ((weight,state):remainders) ix
    | ix - weight <= 0   = state
    | otherwise = weightedSelect remainders (ix-weight)


select :: [a] -> RandState a
    select as = do
        ix <- state $ randomR (0,length as - 1)
        return $ as !! ix

-- |So is this.
runModel :: [String] -> RandState [String]
runModel inputModel = do
  start <- state . randomR $ bounds fastModel
  iter start where
    fastModel = listArray (0,length inputModel - 1) (map read inputModel)
    iter ix = do
      let (ixString,succs) = fastModel ! ix
      succ <- markovSelect succs
      case succ of
        -1 -> do
          rest <- (state . randomR) (bounds fastModel) >>= (\s -> iter s)
          return $ ixString:rest
        n  -> do
          rest <- iter n
          return $ ixString:rest


   runModel :: Model -> RandState [String]
    runModel (start,wordmap) = iter start where
        iter word = do
            let successors = wordmap ! word
            succ <- select successors
            case succ of
                Nothing -> return [word]
                Just w -> do
                    ws <- iter w
                    return (word:ws)-}