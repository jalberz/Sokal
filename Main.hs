{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-04-30
-}

{-
Main.hs 
Below is the main IO function that ties together the various files
of the Sokal Lab. This is broken into four files:
	Main.hs - primary IO coordinator
	Suck.hs - sucks from urls.txt
	MakeModel.hs - helper for model making
	Spew.hs - spews to stdout
To issue a spew command, merely type in: ./spew <desired word count int>


main :: IO ()
main = do
	args <- getArgs
	case (length args) of
		1 -> do
			executeSuck
			executeSpew $ read (arg !! 1)
		_ -> putStrLn "Incorrect number of arguments, please check your input"


Unfortunately, I haven't been able to get the second order Markov model to 
function properly yet... hopefully a working version will be on my git repository
soon.
