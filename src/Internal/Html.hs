{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}

module Internal.Html
    ( checkChordSpacing
    , interleave
    , chord_
    , createChord
    , line
    ) where

import           Prelude hiding (length)
import           Lucid
import           Data.Text (Text, unpack, pack, length)


-- this still needs some work !!!
-- wrapper function for cCS because that function starts calulating padding
-- starting for the second Chord
checkChordSpacing :: [Text] -> [Text] -> [Int]
checkChordSpacing _    []     = []
checkChordSpacing text chords = 0:cCS text chords

cCS :: [Text] -> [Text] -> [Int]
cCS (_:t:ts) (c1:c2:cs) 
    | length c1 > length t  = (15 * (length c1 - length t)):cCS (t:ts) (c2:cs) 
    | length c1 == length t = 5:cCS (t:ts) (c2:cs)
    | otherwise = 0:cCS (t:ts) (c2:cs)
cCS _ _ = []


interleave :: [a] -> [a] -> [a]
interleave as     []     = as
interleave []     bs     = bs
interleave (a:as) (b:bs) = a:b:interleave as bs


chord_ :: Term a r => a -> r
chord_ = termWith "span" [class_ "chord"]


createChord :: Int -> Text -> Html ()
createChord _       ""    = ""
createChord padding chord | padding == 0 = chord_ (toHtml chord) 
                          | otherwise    = chord_ [style_ pad] (toHtml chord)
        where pad = pack ("padding-left: " ++ show padding ++ "px;")


line :: [Text] -> [Text] -> Html ()
line []         []     = ""
line [text]     ["!"]  = i_ (toHtml text)
line textPieces chords = mconcat (interleave htmlText htmlChords ++ [br_ []])
    where padding    = checkChordSpacing textPieces chords
          htmlText   = map toHtml textPieces
          htmlChords = map (uncurry createChord) (zip padding chords)
