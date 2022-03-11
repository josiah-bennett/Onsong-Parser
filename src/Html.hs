{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}

module Html
    ( render
    , header
    , heading
    , metadata
    , section
    , copyright
--    , keywords
    , footer
    ) where

import Control.Monad (forM_)

import Lucid


--header :: ToHtml a => a -> Html ()
header filename = head_ (do
            meta_ [charset_ "UTF-8"]
            meta_ [name_ "viewport", content_ "width=device-width initial-scale=1"]
            link_ [rel_ "stylesheet", href_ "./css/style.css"]
            title_ (toHtml filename)
            meta_ [name_ "description", content_ "chord sheet generated with Onsong-Parser"])


--heading :: ToHtml a => a -> a -> Html ()
heading title artist = h1_ (toHtml title) <> h2_ (toHtml artist)


-- tagMap is supposed to be a list of tuples with
-- the values (key "tag_name", value "tag_value")
metadata :: [(String, String)] -> Html ()
metadata tagMap = ul_ [id_ "metadata"] (forM_ tagMap (li_ . toHtml . concatPair))
    where concatPair (key, value) = key ++ ": " ++ value


chord :: ToHtml a => a -> Html ()
chord c = span_ [class_ "chord"] (toHtml c)


interleave :: [a] -> [a] -> [a]
interleave (a:as) (b:bs) = a:b:interleave as bs
interleave as     []     = as
interleave []     bs     = bs

--line :: ToHtml a => [a] -> [a] -> Html ()
line textPieces chords = mconcat (interleave htmlText htmlChords ++ [br_ []])
    where htmlText   = map toHtml textPieces
          htmlChords = map chord chords

--section :: ToHtml a => a -> [([a],[a])] -> Html ()
section header content = h3_ (toHtml header) <> p_ (forM_ content (uncurry line))


copyright :: ToHtml a => a -> Html ()
copyright text = p_ [id_ "copyright"] (toHtml text)


--keywords text = do
--    something here...


--footer :: T.Text -> Html ()
footer home = footer_ (hr_ [] <> a_ [href_ home] "Home")


render a b = renderText (heading a b)
