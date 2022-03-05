module Main where

import Onsong

import qualified Data.Text as T
import qualified Data.Text.IO as IO

-- read an (arbitrary) file and split it into Paragraphs
readParagraph :: FilePath -> IO [Paragraph]
readParagraph filename = do 
    content <- IO.readFile filename
    return (contentToParagraph content)
        where contentToParagraph = filter (/=[]) . splitParagraph . map (T.strip) . T.lines . T.filter (/='\r')
    

-- write the parsed Paragraphs to a file
writeParagraph :: FilePath -> [Paragraph] -> IO ()
writeParagraph filename parsedSong = do
    let content = (T.unlines . concat) parsedSong
    IO.writeFile filename content


-- append the parsed Paragraphs to an existing file
appendParagraph :: FilePath -> [Paragraph] -> IO ()
appendParagraph filename parsedSong = do
    let content = (T.unlines . concat) parsedSong
    IO.appendFile filename content


main :: IO ()
main = do 
    putStrLn "Enter a Filename:"
    filename <- getLine
    content <- readParagraph (filename ++ ".onsong")
    let song = map parseSongParagraph (tail content)
    writeParagraph (filename ++ ".html") song

