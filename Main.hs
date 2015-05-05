{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-05-05
-}

{-
Main.hs 
Below is the main IO function that ties together the various files
of the Sokal Lab. This is broken into four files:
	Main.hs - primary IO coordinator
	Suck.hs - sucks from urls.txt
	MakeModel.hs - helper for model making
	Spew.hs - spews to stdout
	as well as Sokal.cabal
-}

import Distribution.Simple
import Suck
import Spew
import System.IO
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	case (length args) of
		1 -> do
			executeSuck
			executeSpew $ read (args !! 1)
		_ -> putStrLn "Incorrect number of arguments, please check your input"


{-
A GitHub repository for this program can be found at https://github.com/jalberz/Sokal.git
There you can find build/execute instructions as well as test cases.
-}
