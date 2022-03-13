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
    , parseMetadata
    , createMetadataList
    ) where

import           Data.Either
import           Data.Text       (Text, pack, unpack)
import qualified Data.Text       as T
import           Text.Parsec

import           Internal.Onsong

-- create an alias for [Text]
type Paragraph = [Text]

splitParagraph :: [Text] -> [Paragraph]
splitParagraph songLines = splitAts (findParagraph songLines) songLines

-------------------------------------------------------------------------------
-- Functions to parse the 'tail' of the file or the actual song
-- this should work for both .onsong and .chopro files
-- Section parsing

parseHeader :: Paragraph -> Text
parseHeader (x:_) = (pack . fromRight "" . parse pHeader "(source)" . unpack) x

parseSection :: Paragraph -> [([Text], [Text])]
parseSection (x:xs) = case parse pHeader "(source)" (unpack x) of
            Right _ -> map parseLine xs
            Left  _ -> map parseLine (x:xs)

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
createMetadataList tags metadata = map (`parseMetadata` metadata) tags


-- I can already see this implementation break, for some obscure song (but for now it works) !!!
parseCopyright :: Paragraph -> Text
parseCopyright metadata | null t    = T.empty
                        | otherwise = (T.strip . pack . fromRight t . parse (manyTill anyChar (try (char '('))) "(source)") t
                where t = parseTag "Copyright" metadata

