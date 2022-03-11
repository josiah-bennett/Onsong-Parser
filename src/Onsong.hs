{-# LANGUAGE OverloadedStrings #-}

module Onsong
    ( Paragraph
    , splitParagraph
    , parseSection
    , parseHeader
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

parseSectionLine :: Text -> ([Text], [Text])
parseSectionLine = unzip . parseLine . parseLine1 . unpack

-- this whole block is pretty unreadable, unmaintainable mess...
-- it works but would greatly benefit from clean up sometime... TODO !!!
parseLine :: [String] -> [(Text, Text)]
parseLine [] = []
parseLine [a] = [(pack a, "")]
parseLine (a1:a2:as) = (pack a1, pack a2):(parseLine as)

-- double recursion here !!! watch out !!!
parseLine1 :: String -> [String]
parseLine1 line = case (parse p1 "(source)" line) of
        Right t -> t:(parseLine2 (drop (length t + 1) line))
        Left  _ | null line -> []
                | otherwise -> [line]
    where p1 = manyTill anyChar (try (char '['))

parseLine2 :: String -> [String]
parseLine2 line = case (parse p2 "(source)" line) of
        Right t -> t:(parseLine1 (drop (length t + 1) line))
        Left  _ -> []
    where p2 = manyTill anyChar (try (char ']'))


parseHeader :: Paragraph -> Text
parseHeader (p:_) = case (parse (parseSectionHeader) "(source)" (unpack p)) of
            Right t -> pack t
            Left  _ -> ""

parseSection :: Paragraph -> [([Text], [Text])]
parseSection (p:ps) = case (parse (parseSectionHeader) "(source)" (unpack p)) of
            Right _ -> map (parseSectionLine) ps
            Left  _ -> map (parseSectionLine) (p:ps)

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

