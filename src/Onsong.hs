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

findParagraph = filter (/=0) . zipWith (*) [1..] . map (fromEnum . T.null)

-------------------------------------------------------------------------------
-- Functions to parse the 'tail' of the file or the actual song
-- this should work for both .onsong and .chopro files
-- Section parsing

-- this functions has its flaws ... (if there is still content after the ':' it gets cut off) [problem for future me...]
parseSectionHeader :: Parsec String () String 
parseSectionHeader = manyTill anyChar (try (char ':'))

parseSectionLine :: Text -> ([Text], [Text])
parseSectionLine = unzip . parseLine . parseText . unpack

-- this whole block is pretty unreadable, unmaintainable mess...
-- it works but would greatly benefit from clean up sometime... TODO !!!
parseLine :: [String] -> [(Text, Text)]
parseLine [] = []
parseLine [a] = [(pack a, "")]
parseLine (a1:a2:as) = (pack a1, pack a2):(parseLine as)

-- double recursion here !!! watch out !!!
p :: Char -> Parsec String () String
p c = manyTill anyChar (try (char c))

parseText :: String -> [String]
parseText line = case (parse (p '[') "(source)" line) of
        Right t -> t:(parseChord (drop (length t + 1) line))
        Left  _ | null line -> []
                | otherwise -> [line]

parseChord :: String -> [String]
parseChord line = case (parse (p ']') "(source)" line) of
        Right t -> t:(parseText (drop (length t + 1) line))
        Left  _ -> []


parseHeader :: Paragraph -> Text
parseHeader (p:_) = (pack . fromRight "" . parse (parseSectionHeader) "(source)" . unpack) p

parseSection :: Paragraph -> [([Text], [Text])]
parseSection (p:ps) = case (parse (parseSectionHeader) "(source)" (unpack p)) of
            Right _ -> map (parseSectionLine) ps
            Left  _ -> map (parseSectionLine) (p:ps)

-------------------------------------------------------------------------------
-- Metadata parsing
tag' :: String -> Parsec String () String
tag' s = string s >> char ':' >> many space >> many anyChar


parseTag :: String -> Paragraph -> String
parseTag _ [] = ""
parseTag tag (x:xs) = (fromRight (parseTag tag xs) . parse (tag' tag) "(source)" . unpack) x


parseTitle :: Paragraph -> Text
parseTitle metadata | null t    = head metadata
                    | otherwise = pack t
            where t = parseTag "Title" metadata


parseArtist :: Paragraph -> Text
parseArtist metadata | null tag  = case implicit of
                            Right _ -> "Unknown Artist"
                            Left  _ -> line2
                     | otherwise = pack tag
            where tag      = parseTag "Artist" metadata
                  implicit = parse (manyTill anyChar (try (char ':'))) "(source)" line2
                  line2 | (null . tail) metadata = ":"
                        | otherwise = (head . tail) metadata


parseMetadata :: String -> Paragraph -> (String, String)
parseMetadata tag metadata | null t    = (tag, "")
                           | otherwise = (tag, t)
                where t = parseTag tag metadata

createMetadataList :: [String] -> Paragraph -> [(String, String)]
createMetadataList tags metadata = map (flip parseMetadata metadata) tags


-- I can already see this implementation break, for some obscure song (but for now it works) !!!
parseCopyright :: Paragraph -> Text
parseCopyright metadata | null t    = T.empty
                        | otherwise = (T.strip . pack . fromRight t . parse (manyTill anyChar (try (char '('))) "(source)") t
                where t = parseTag "Copyright" metadata

