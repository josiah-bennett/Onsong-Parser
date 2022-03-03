module Onsong
    where

import System.IO
import Control.Exception

removeKeyword :: String -> String -> String
removeKeyword [] content = content
removeKeyword _ [] = []
removeKeyword (x:xs) (y:ys) 
    | x == y    = removeKeyword xs ys
    | otherwise = y:(removeKeyword (x:xs) ys)

removeFirstLine = removeKeyword "{new_song}"
removeTitle     = removeKeyword "{title:}"
removeArtist    = removeKeyword "{subtitle:}"
removeKey       = removeKeyword "{key:}"
removeTempo     = removeKeyword "{tempo:}"
removeTime      = removeKeyword "{time:}"
removeCopyright = removeKeyword "{copyright:}"

createHTMLTag :: Bool -> String -> String
createHTMLTag closing tag
    | closing = foldr (++) "" ["</", tag, ">"]
    | otherwise = foldr (++) "" ["<", tag, ">"]

createClosingTag = createHTMLTag True
createOpeningTag = createHTMLTag False

createChordStructure :: String -> String
createChordStructure chord = 
    (createOpeningTag "span class=\"chord\"") ++ (createOpeningTag "span class=\"text\"") 
    ++ (removeKeyword "[]" chord) ++ (createClosingTag "span") ++ (createClosingTag "span")

convertToHTML :: String -> String -> (String -> String) -> String
convertToHTML content tag removeFunc = 
    (createOpeningTag tag) ++ (removeFunc content) ++ (createClosingTag tag)

main :: IO ()
main = do
    --filename <- getLine
    let filename = "aidz.chopro"
    bracket (openFile filename ReadMode) hClose
            (\h -> do content <- hGetContents h
                      putStrLn (show (fileToLines content))
                      putStrLn ("")
                      putStrLn (show (parseToHTML (fileToLines content))))

fileToLines :: String -> [(Int, String)]
fileToLines file = zip [1..] (lines (filter ((/=) '\r') file))

parseToHTML :: [(Int, String)] -> [String]
parseToHTML [] = []
parseToHTML (line:songLines) = (x line):(parseToHTML songLines)
    where x line = 
        case (fst line) of
            1 -> ""
            2 -> convertToHTML (snd line) "h1" removeTitle
            3 -> convertToHTML (snd line) "h4" removeArtist
            4 -> convertToHTML (snd line) "p" removeKey
            _ -> snd line

{-
readLine handle = do
    eof <- hIsEOF handle
    if eof then return [""]
    else do
        line <- hGetLine handle
        line:(readLine handle)
-}
{-
doStuff handle = do
    line <- readLine handle
    putStrLn (removeFirstLine line)
    line <- readLine handle
    --putStrLn ((createOpeningTag "h1") ++ (removeTitle line) ++ (createClosingTag "h1"))
    putStr ((++) (removeTitle line) (createClosingTag "h1")) -- Something here is really wrong !!!
-}
