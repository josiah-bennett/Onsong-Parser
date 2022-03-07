module Main where

import Onsong

import System.Environment

import qualified Data.Text as T
import qualified Data.Text.IO as IO

-- read an (arbitrary) file and split it into Paragraphs
readParagraph :: FilePath -> IO [Paragraph]
readParagraph filename = do 
    content <- IO.readFile filename
    return (contentToParagraph content)
        where contentToParagraph = filter (/=[]) . splitParagraph . map (T.strip) . T.lines . T.filter (/='\r')


-- write the parsed Paragraphs to a file
writeParagraph :: FilePath -> Paragraph -> IO ()
writeParagraph filename parsedSong = do
    let content = T.unlines parsedSong
    IO.writeFile filename content


-- append the parsed Paragraphs to an existing file
appendParagraph :: FilePath -> Paragraph -> IO ()
appendParagraph filename parsedSong = do
    let content = T.unlines parsedSong
    IO.appendFile filename content


putSongTogether :: [String] -> [Paragraph] -> Paragraph
putSongTogether tags c = concat [createHeader h, createDataList tags h, wrapSong s, [createCopyright h]]
    where h = head c; s = tail c

{- --command line integration if wanted for bulk operations
main :: IO ()
main = do
    filename <- getArgs
    content <- readParagraph (head filename)
    let song = putSongTogether ["Key", "Tempo", "Time", "Duration"] content
    writeParagraph (((truncFilename . head) filename) ++ ".html") song
    where truncFilename = takeWhile (/='.')
-}
-- {- --default main function
main :: IO ()
main = do 
    putStrLn "Enter a Filename:"
    filename <- getLine
    content <- readParagraph (filename ++ ".onsong")
    let song = putSongTogether ["Key", "Tempo", "Time", "Duration"] content
    writeParagraph (filename ++ ".html") song
-- -}

