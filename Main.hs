{-# LANGUAGE OverloadedStrings #-}

module Main 
    ( main
    , readParagraph
    , createHtml
    ) where

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


createHtml :: [Paragraph] -> Html ()
createHtml content = doctype_ <> html_ 
                      (do header 
                            (parseTitle mdata)
                          body_
                            (do main_
                                  (do heading (parseTitle mdata) (parseArtist mdata)
                                      metadata (createMetadataList mdataList mdata)
                                      div_ [id_ "song", class_ "show-chords"] 
                                        (do mapM_ (\s -> section (parseHeader s) (parseSection s)) song))
                                copyright (parseCopyright mdata)
                                footer ("../index.html")))
        where mdata = head content
              mdataList = ["Key", "Time", "Tempo", "Duration"]
              song = tail content


-- command line integration if wanted for bulk operations
-- takes two arguments the .onsong file and the filename to write to
main :: IO ()
main = do
    filenames <- getArgs
    content <- readParagraph (head filenames)
    let song = createHtml content
    renderToFile ((head . tail) filenames) song

--default main function
{-
main :: IO ()
main = do 
    putStrLn "Enter a Filename:"
    filename <- getLine
    content <- readParagraph (filename ++ ".onsong")
    let song = createHtml content
    renderToFile (filename ++ ".html") song
-}

