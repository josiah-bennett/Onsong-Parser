{-# LANGUAGE OverloadedStrings #-}

module Main where

import Onsong
import Html

import Lucid

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


createHtml :: [Paragraph] -> Html ()
createHtml content = html_ 
                      (do header 
                            (parseTitle mdata)
                          body_
                            (do main_
                                  (do heading (parseTitle mdata) (parseArtist mdata)
                                      metadata (createMetadataList mdataList mdata)
                                      div_ [id_ "song"] 
                                        (do mapM_ (\s -> section (parseHeader s) (parseSection s)) song)
                                      copyright (parseCopyright mdata))
                                footer (".")))
        where mdata = head content
              mdataList = ["Key", "Time", "Tempo", "Duration"]
              song = tail content


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
-- TODO this mess desperatly needs fixing !!!
main :: IO ()
main = do 
    putStrLn "Enter a Filename:"
    filename <- getLine
    content <- readParagraph (filename ++ ".onsong")
    let song = createHtml content
    renderToFile (filename ++ ".html") song
-- -}

