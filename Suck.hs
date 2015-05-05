{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-04-15
-}

module Suck where

--import Distribution.Simple
import System.IO
import Data.List
import Network.HTTP
import Text.HTML.TagSoup
import Text.StringLike
import Data.Char
import MakeModel

{-
executeSuck opens a file, takes the urls from it and siphons the text, and 
subsequently removes HTML tags from it. It calls upon makeModel to make the
text into a frequency sorted mapping that is written to sokal.model
-}
executeSuck :: IO ()
executeSuck = do
    handle <- openFile "urls.txt" ReadMode
    putStrLn "opening file"
    contents <- (hGetContents handle) >>= (return . lines)
    putStrLn "reading urls...Please wait"
    rawtext <- mapM (\u -> simpleHTTP (getRequest u) >>= getResponseBody) contents
    putStrLn "following urls...Please wait"
    let harvested = map (harvest) rawtext
    putStrLn "Sucessful harvest:"
    let list = unlines . map show $ (makeModel harvested)
    writeFile "sokal.model" list
    putStrLn "Model constructed"
    hClose handle


{-
The harvest function takes everything in between the beginning of
the body div and the beginning of the back div - trimming the interior
of any HTML
-}
harvest ::  String -> String
harvest s =
    let
        tags = parseTags s
        paragraphs = map clean $ partitions (~== TagOpen "p" [])
            $ takeWhile (~/= TagOpen "div" [("id","back")])
            $ dropWhile (~/= TagOpen "div" [("id","body")]) tags
    in
        unlines paragraphs where
            clean :: [Tag String] -> String
            clean = unwords
                . words
                . filter isAscii
                . innerText
                . filter isTagText
