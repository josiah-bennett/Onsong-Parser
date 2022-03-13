{-# LANGUAGE OverloadedStrings #-}

module OnsongSpec (spec) where

import Onsong
import Internal.Onsong
import Test.Hspec

import qualified Data.Text as T
import Text.Parsec


spec :: Spec
spec = do
    findParagraphSpec
    splitAtsSpec
    splitParagraphSpec

    pHeaderSpec
    pBracetsSpec

    parseTextChordSpec
    splitLineSpec


findParagraphSpec :: Spec
findParagraphSpec = describe "Test the function that finds all of the empty lines to split a text into paragraphs" $ do
    context "When no text is passed" $
        it "should return an empty list" $
            findParagraph ([] :: [T.Text]) `shouldBe` []
    context "When the text is empty" $ do
        it "should return the index 1" $
            findParagraph [""] `shouldBe` [1]
        it "should return a list with the indicies for the empty lines [1..5]" $
            findParagraph ["","","","",""] `shouldBe` [1..5]
    context "When the text only contains one paragraph without empty lines" $
        it "should return an empty list" $ do
            findParagraph ["line1","line2","line3","line4","line5"] `shouldBe` []
            findParagraph ["line1","line2","line3","line4","line5"] `shouldBe` []
    context "When the text contains a single paragraph with newlines at the beginning or end" $
        it "should return a list with the unneccesary newlines" $ do
            findParagraph ["","line1","line2"] `shouldBe` [1]
            findParagraph ["","","","line1","line2"] `shouldBe` [1,2,3]
            findParagraph ["","","line1","line2",""] `shouldBe` [1,2,5]
            findParagraph ["line1","line2","",""] `shouldBe` [3,4]
    context "When the text contains two/multiple paragraphs" $
        it "should return a list of the linenumbers from the lines that are only newlines" $ do
            findParagraph ["line1","line2","","line3","line4"] `shouldBe` [3]
            findParagraph ["","","line1","","line2",""] `shouldBe` [1,2,4,6]


splitAtsSpec :: Spec
splitAtsSpec = describe "Test the fuction which splits a list at multiple places" $ do
    context "When no arguments are passed" $
        it "should return empty list" $
            splitAts [] ([] :: [String]) `shouldBe` [[]]
    context "When there is nowhere to split at" $
        it "should return the whole list unsplit" $
            splitAts [] [1..10] `shouldBe` [[1..10]]
    context "When there is no list to split" $
        it "shoud return a list of empty lists" $
            splitAts [1,3,5] ([] :: [String]) `shouldBe` [[], [], [], []]
    context "When you need to split the text once" $
        it "should return two sublists without the list item where the split occured at" $
            splitAts [3] [1..10] `shouldBe` [[1,2],[4,5,6,7,8,9,10]]
    context "When you split the text multiple times" $
        it "should return multiple sublists all without the list elements where the split occured" $ do
            splitAts [3,6] [1..10] `shouldBe` [[1,2],[4,5],[7,8,9,10]]
            splitAts [3,4,7] [1..10] `shouldBe` [[1,2],[],[5,6],[8,9,10]]


splitParagraphSpec :: Spec
splitParagraphSpec = describe "Test the function that splits a given text into paragraphs" $ do
    context "When the text is empty" $
        it "should return no paragraphs" $
            splitParagraph [] `shouldBe` [[]]
    context "When the Text contains only newlines" $
        it "should a whole bunch of empty paragraphs" $
            splitParagraph ["","","","","",""] `shouldBe` [[],[],[],[],[],[],[]]
    context "When the text contains one paragraph" $
        it "should return a list with said paragraph and empty lists for extra empty lines" $
            splitParagraph ["", "line1", "line2", "line3", "line4", "", ""] `shouldBe` [[], ["line1", "line2", "line3", "line4"], [], []]
    context "When the text contains multiple paragraphs" $
        it "should return a list with those paragraphs as sublists and empty lines as empty lists" $
            splitParagraph ["line1", "line2", "line3", "line4", "", "", "line5", "line6", "line7", "", "line8", "line9", "line10", ""] `shouldBe` [["line1", "line2", "line3", "line4"], [], ["line5", "line6", "line7"], ["line8", "line9", "line10"], []]

-------------------------------------------------

parseString :: Parsec String () String -> String -> String
parseString p input =
    case parse p "Example" input of
        Left{}    -> error "Parse failure"
        Right str -> str

parseFail :: Parsec String () String -> String -> String
parseFail p input =
      case parse p "Example" input of
        Left{}  -> "no parse"
        Right _ -> error "Parsed but shouldn't"


pHeaderSpec :: Spec
pHeaderSpec = describe "Test the function that parses the first line of every song section" $ do
    context "When the first line is empty" $
        it "should throw an error" $
            parseFail pHeader "" `shouldBe` "no parse"
    context "When the first line doesn't contain a ':'" $
        it "should throw an error" $
            parseFail pHeader "some random testing text" `shouldBe` "no parse"
    context "When the line ends on a ':'" $
        it "should parse the whole line except the ':'" $
            parseString pHeader "Verse 3:" `shouldBe` "Verse 3"
    context "When the line continues after the ':'" $
        it "should just parse everything before the ':'" $
            parseString pHeader "Some text: adn it continues right here" `shouldBe` "Some text"

pBracetsSpec :: Spec
pBracetsSpec = describe "Test the function that parses square bracets to identify chords in song lines" $ do
    context "When no string is passed" $
        it "should throw an error" $
            parseFail (pBracets '[') "" `shouldBe` "no parse"
    context "When no Bracets are found" $
        it "should throw an error" $
            parseFail (pBracets ']') "text[D" `shouldBe` "no parse"
    context "When a Bracet is found" $
        it "should parse everything upto that character" $ do
            parseString (pBracets '[') "text[G]text[" `shouldBe` "text"
            parseString (pBracets ']') "text[G]text[" `shouldBe` "text[G"


-- parseText and parseChord have to be tested together because of their recursive behaviour
parseTextChordSpec :: Spec
parseTextChordSpec = describe "Test the combo of parseText and parseChord" $ do
    context "When the line passed is empty" $
        it "should return an empty list" $
            parseText "" `shouldBe` []
    context "When the line is just a chord like [D] or [Gsus]" $
        it "should return a list like [\"\", \"D\"]" $
            parseText "[D]" `shouldBe` ["", "D"]
    context "When the line is just text" $
        it "should return a list with the text" $
            parseText "some text" `shouldBe` ["some text"]
    context "When the line has some text and chords" $
        it "should separate those in the list" $ do
            parseText "text[D]text[G]text[A]" `shouldBe` ["text", "D", "text", "G", "text", "A"]
            parseText "text[D]text[G]text" `shouldBe` ["text", "D", "text", "G", "text"]
            parseText "[D]text[G]text[A]" `shouldBe` ["", "D", "text", "G", "text", "A"]
            parseText "text[D][G]text[A]" `shouldBe` ["text", "D", "", "G", "text", "A"]
            parseText "text[D]text[G][A]" `shouldBe` ["text", "D", "text", "G", "", "A"]


splitLineSpec :: Spec
splitLineSpec = describe "Test the function that splits a list with alternating text and chords into a list of tuples" $ do
    context "When the passed list is empty" $
        it "should return an empty list" $
            splitLine [] `shouldBe` []
    context "When there is only one text element in the list" $
        it "should return that element with an empty string as the chord" $
            splitLine ["text"] `shouldBe` [("text", "")]
    context "When there are two elements in the list" $
        it "should create a list with one tuple element" $
            splitLine ["text", "D"] `shouldBe` [("text", "D")]
    context "When there are more than two elements in the list" $
        it "should 'just work'" $
            splitLine ["", "D", "text", "G", "text", "A", "text"] `shouldBe` [("", "D"), ("text", "G"), ("text", "A"), ("text", "")]

