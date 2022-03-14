{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}

module Internal.Html
    ( checkChordSpacing
    , interleave
    , chord_
    , line
    ) where

import           Prelude hiding (length)
import           Lucid
import           Data.Text (Text, unpack, pack, length)


checkChordSpacing :: [Text] -> [Text] -> [Int]
checkChordSpacing []  [c] = []
checkChordSpacing [t] [c] = []
checkChordSpacing [t] [] = []
checkChordSpacing [t1,t2] [c] = []
checkChordSpacing (_:t:ts) (c1:c2:cs) 
    | length c1 >= length t = (14 * (length c1 - length t)):checkChordSpacing (t:ts) (c2:cs) 
    | otherwise = 0:checkChordSpacing (t:ts) (c2:cs)


interleave :: [a] -> [a] -> [a]
interleave as     []     = as
interleave []     bs     = bs
interleave (a:as) (b:bs) = a:b:interleave as bs


chord_ :: Term a r => a -> r
chord_ = termWith "span" [class_ "chord"]


addPadding :: Int -> Text -> Html ()
addPadding padding chord | padding == 0 = chord_ (toHtml chord) 
                         | otherwise    = chord_ [style_ pad] (toHtml chord)
        where pad = pack ("padding-left: " ++ show padding ++ ";")


line :: [Text] -> [Text] -> Html ()
line [] [] = ""
line textPieces chords = mconcat (interleave htmlText htmlChords ++ [br_ []])
    where padding    = 0:checkChordSpacing textPieces chords
          htmlText   = map toHtml textPieces
          htmlChords = map (uncurry addPadding) (zip padding chords)
