{-
Jacob Albers
CMSC 22311 - Functional Systems in Haskell
Instructors: Stuart Kurtz & Jakub Tucholski
2015-04-15
-}

module Suck where

import System.IO
import Data.List
import Network.HTTP
import Text.HTML.TagSoup as TS
import Data.Char
import Model

{-
executeSuck opens a file, takes the urls from it and siphons the text, and 
subsequently removes HTML tags from it. It calls upon makeModel to make the
text into a frequency sorted mapping that is written to sokal.model
-}       
executeSuck :: IO ()
executeSuck = do
    handle <- openFile "urls.txt" ReadMode
    contents <- hGetContents handle
    let split = lines contents
        rawtext = head $ map (\u -> simpleHTTP (getRequest u) >>= getResponseBody) split
    cleaned <- harvest (filter isAscii rawtext)
    writeFile "sokal.model" (makeModel cleaned)
    hClose handle

{-
The harvest function takes everything in between the beginning of
the body div and the beginning of the bios div - trimming the interior
of any HTML
-}
harvest :: String -> String
harvest s = cleantext where
    parsed = TS.canonicalizeTags $ TS.parseTags s
    openb = dropWhile (/= TS.TagOpen "div" [("id","body")]) parsed
    closeb = takeWhile (/= TS.TagOpen "div" [("class","bio")]) openb
    cleantext = removeHTML $ grabParagraphs closeb 1

--A helper function for harvest
grabParagraphs t i
    | i == 0 = []
    | otherwise = 
        case t of
            (TagOpen "div" attr):tags -> (TagOpen "div" attr)
                                        :(grabParagraphs tags (i + 1))
            (TagClose "div"):tags     -> (TagClose "div")
                                        :(grabParagraphs tags (i - 1) )
            tag:tags                  -> tag:(grabParagraphs tags i)
            []                        -> []

--A helper function to remove html from the interior
removeHTML tags = case tags of 
    [] -> []
    t:ts -> if (t ~== TagText "")
                then
                    TS.fromTagText t ++ removeHTML ts
                else
                    removeHTML ts



