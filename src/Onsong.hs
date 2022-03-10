{-# LANGUAGE OverloadedStrings #-}

module Onsong
    ( Paragraph
    , splitParagraph
    , wrapSong
    , parseTag
    , parseTitle
    , parseArtist
    , parseCopyright
    , createMetadataList
    ) where

import Data.Text (Text, pack, unpack)
import Data.Either
import Text.Parsec

import qualified Data.Text as T

-- create an alias for [Text]
type Paragraph = [Text]

splitParagraph :: [Text] -> [Paragraph]
splitParagraph songLines = splitAts (findParagraph songLines) songLines

splitAts xs text = split text (reverse xs)
    where split t [] = [t]
          split t (y:ys) = (split (take (y-1) t) ys) ++ [drop y t]

-- or the alternative:
{-
splitAts xs text = split text (diffList xs)
    where split t [] = [t]
          split t (y:ys) = (take y t):(split (drop y t) ys)

-- diffList subtracts the first item in the list from the rest for all elements
diffList = f . (0:)
    where f [_] = []
          f (a:b:xs) = (b-a):(f . (b:)) xs
-}

findParagraph = filter (/=0) . zipWith (*) [1..] . map (fromEnum . T.null)

-- or again the alternatives:
{-
findParagraph = filter (/=0) . map (\(i,x) -> i * (fromEnum . T.null) x) . zip [1..]

findParagraph = filter (/=0) . map (uncurry(*)) . zip [1..] . map (fromEnum . T.null)
-}

-- Functions to parse the 'tail' of the file or the actual song
-- this should work for both .onsong and .chopro files
-- Section parsing

-- this functions has its flaws ... (if there is still content after the ':' it gets cut off) [problem for future me...]
parseSectionHeader :: Parsec String () String 
parseSectionHeader = manyTill anyChar (try (char ':'))

parseSectionLine :: Text -> Text -- TODO change output type to ([Text], [Text]) the first list is the text the second the chords...
parseSectionLine line = (T.replace (pack "[") openingTag . T.replace (pack "]") closingTag) (T.append line (pack "<br>"))
    where openingTag = pack "<span class=\"chord\">"
          closingTag = pack "</span>"

parseSongParagraph :: Paragraph -> Paragraph
parseSongParagraph (p:ps) | (isRight . parseHeader) p = (header p):(parseSection ps)
                          | otherwise                 = parseSection (p:ps)
    where parseSection s = (map (parseSongLine) s) ++ [pack "</p>\n"]
          parseHeader  h = parse (parseParagraphHeader) "(source)" (unpack h)
          header h = case (parseHeader h) of
              Right text -> pack ("<h3>" ++ text ++ "</h3>\n<p>")
              Left _ -> pack "<p>"

wrapSong :: [Paragraph] -> Paragraph
wrapSong song = concat [[pack "<div id=\"song\">"], concatMap parseSongParagraph song, [pack "</div>\n"]]

-------------------------------------------------------------------------------
-- Metadata parsing

parseTag :: String -> Paragraph -> [String]
parseTag tag = filter (/="") . map (fromRight "" . parse t "(source)" . unpack)
    where t = string tag >> char ':' >> many space >> many anyChar


parseTitle :: Paragraph -> Text
parseTitle metadata | null t    = head metadata
                    | otherwise = (pack . head) t
            where t = parseTag "Title" metadata


parseArtist :: Paragraph -> Text
parseArtist metadata | null t    = case second of
                            Right _ -> "Unknown Artist"
                            Left  _ -> line2 metadata
                     | otherwise = (pack . head) t
            where t   = parseTag "Artist" metadata
                  second = parse (manyTill anyChar (try (char ':'))) "(source)" (line2 metadata)
                  line2 line | (null . tail) line = ":"
                             | otherwise          = (head . tail) line


parseMetadata :: String -> Paragraph -> (String, String)
parseMetadata tag metadata | null t    = (tag, "")
                           | otherwise = (tag, head t)
                where t = parseTag tag metadata

createMetadataList :: [String] -> Paragraph -> [(String, String)]
createMetadataList tags metadata = map (\t -> parseMetadata t metadata) tags


-- I can already see this implementation break, for some obscure song (but for now it works) !!!
parseCopyright :: Paragraph -> Text
parseCopyright metadata | null t    = T.empty
                        | otherwise = (T.strip . pack . fromRight (head t) . parse (manyTill anyChar (try (char '('))) "(source)") (head t)
                where t = parseTag "Copyright" metadata

