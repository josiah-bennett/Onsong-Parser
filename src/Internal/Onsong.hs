{-# LANGUAGE OverloadedStrings #-}

module Internal.Onsong
    ( splitAts
    , findParagraph
    , pHeader
    , pBracets
    , parseText
    , parseChord
    , splitLine
    , parseLine
    ) where

import           Data.Text   (Text, pack, unpack)
import qualified Data.Text   as T
import           Text.Parsec

-- split a text at multiple positions
splitAts :: [Int] -> [a] -> [[a]]
splitAts xs text = split text (reverse xs)
    where split t []     = [t]
          split t (y:ys) = split (take (y-1) t) ys ++ [drop y t]

findParagraph :: [Text] -> [Int]
findParagraph = filter (/=0) . zipWith (*) [1..] . map (fromEnum . T.null)

---------------------------------------------------------------------
-- functions for parsing a song section such as a verse or chorus

-- this function has its flaws ... (if there is still content after the ':' it gets cut off) [problem for future me...]
pHeader :: Parsec String () String
pHeader = manyTill anyChar (try (char ':'))

parseLine :: Text -> ([Text], [Text])
parseLine = unzip . splitLine . parseText . unpack

-- this whole block is pretty unreadable, unmaintainable mess...
-- it works but would greatly benefit from clean up sometime... TODO !!!
splitLine :: [String] -> [(Text, Text)]
splitLine []         = []
splitLine [a]        = [(pack a, "")]
splitLine (a1:a2:as) = (pack a1, pack a2):splitLine as

-- double recursion here !!! watch out !!!
pBracets :: Char -> Parsec String () String
pBracets c = manyTill anyChar (try (char c))

parseText :: String -> [String]
parseText line = case parse (pBracets '[') "(source)" line of
        Right t -> t:parseChord (drop (length t + 1) line)
        Left  _ | null line -> []
                | otherwise -> [line]

parseChord :: String -> [String]
parseChord line = case parse (pBracets ']') "(source)" line of
        Right t -> t:parseText (drop (length t + 1) line)
        Left  _ -> []

