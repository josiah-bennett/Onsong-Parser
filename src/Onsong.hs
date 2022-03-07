module Onsong
    ( Paragraph
    , splitParagraph
    , wrapSong
    , createHeader
    , createDataList
    , createCopyright
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
-- Metadata is going to be parsed extra
parseParagraphHeader :: Parsec String () String 
parseParagraphHeader = manyTill anyChar (try (char ':'))

parseSongLine :: Text -> Text
parseSongLine line = (T.replace (pack "[") openingTag . T.replace (pack "]") closingTag) (T.append line (pack "<br>"))
    where openingTag = pack "<span class=\"chord\"><span class=\"text\">"
          closingTag = pack "</span></span>"

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
parseData :: String -> Paragraph -> [String]
parseData tag = (filter (/="") . map (fromRight "" . parse p "(source)" . unpack))
    where p = string tag >> char ':' >> many space >> many anyChar

parseTitle :: Paragraph -> Text
parseTitle metadata | null p = head metadata
                    | otherwise = (pack . head) p
    where p = parseData "Title" metadata

line2 = (head . tail)

parseArtist :: Paragraph -> Text
parseArtist metadata | null p = case second of
                          Right _ -> pack "Unknown Artist"
                          Left _ -> (line2) metadata
                     | otherwise = (pack . head) p
    where p = parseData "Artist" metadata
          second = parse (manyTill anyChar (try (char ':'))) "(source)" (line2 metadata)

createHeader :: Paragraph -> [Text]
createHeader metadata = [t, a, T.empty]
    where t = T.concat [pack "<h1>", parseTitle metadata, pack "</h1>"]
          a = T.concat [pack "<h2>", parseArtist metadata, pack "</h2>"]

convertData :: String -> Paragraph -> Text
convertData tag metadata | null p    = T.empty
                         | otherwise = (pack . foldr (++) "") ["<li id=\"", lower tag, "\">", tag, ": ", head p, "</li>"]
    where p = parseData tag metadata
          lower = unpack . T.toLower . pack

createDataList :: [String] -> Paragraph -> [Text]
createDataList dataTags metadata = concat [[pack "<ul id=\"metadata\">"], tags dataTags, [pack "</ul>\n"]]
    where tags = filter (/=T.empty) . map f
          f tag = convertData tag metadata

-- I can already see this implementation break, for some obscure song (but for now it works) !!!
createCopyright :: Paragraph -> Text
createCopyright metadata | null p    = T.empty
                         | otherwise = T.concat [pack "<p id=\"copyright\">", removeParentheses, pack "</p>\n"]
    where p = parseData "Copyright" metadata
          removeParentheses = (T.strip . pack . fromRight (head p) . parse (manyTill anyChar (try (char '('))) "(source)") (head p)

