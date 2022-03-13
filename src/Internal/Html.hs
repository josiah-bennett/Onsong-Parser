{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}

module Internal.Html
    ( interleave
    , chord
    , line
    ) where

import           Lucid


interleave :: [a] -> [a] -> [a]
interleave as     []     = as
interleave []     bs     = bs
interleave (a:as) (b:bs) = a:b:interleave as bs


chord :: ToHtml a => a -> Html ()
chord c = span_ [class_ "chord"] (toHtml c)

--line :: ToHtml a => [a] -> [a] -> Html ()
line [] [] = ""
line textPieces chords = mconcat (interleave htmlText htmlChords ++ [br_ []])
    where htmlText   = map toHtml textPieces
          htmlChords = map chord chords

