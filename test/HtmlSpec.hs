{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}

module HtmlSpec (spec) where

import           Html
import           Internal.Html
import           Test.Hspec

import           Lucid         (renderText, style_, class_)
import           Data.Text     (Text)

spec :: Spec
spec = do
    headerSpec
    headingSpec
    metadataSpec
    interleaveSpec
    chordSpec
    checkChordSpacingSpec
    createChordSpec
    lineSpec
    sectionSpec
    --keywordSpec
    copyrightSpec
    footerSpec


headerSpec :: Spec
headerSpec = describe "'header' -> Tests the header function that generates the html head" $ do
  context "When no title is passed" $
    it "should match the default head without a title" $
      renderText (header ("")) `shouldBe` "<head><meta charset=\"UTF-8\"><meta content=\"width=device-width initial-scale=1\" name=\"viewport\"><link href=\"./css/style.css\" rel=\"stylesheet\"><title></title><meta content=\"chord sheet generated with Onsong-Parser\" name=\"description\"></head>"
  context "When a title is passed" $
    it "should use the name as the website title" $
      renderText (header ("Amazing Grace")) `shouldBe` "<head><meta charset=\"UTF-8\"><meta content=\"width=device-width initial-scale=1\" name=\"viewport\"><link href=\"./css/style.css\" rel=\"stylesheet\"><title>Amazing Grace</title><meta content=\"chord sheet generated with Onsong-Parser\" name=\"description\"></head>"


headingSpec :: Spec
headingSpec = describe "'heading' -> Tests the heading function that generates the title and artist headings" $ do
  context "When some title and artist is passed" $
    it "should match both title and artist to their respective tags" $
      renderText (heading "Amazing Grace" "John Newton") `shouldBe` "<h1>Amazing Grace</h1><h2>John Newton</h2>"
  context "When only a title is passed" $
    it "should leave the h2 tag empty" $
      renderText (heading "Amazing Grace" "") `shouldBe` "<h1>Amazing Grace</h1><h2></h2>"
  context "when only a artist is passed" $
    it "should leave the h1 tag empty" $
      renderText (heading "" "Unknown Artist") `shouldBe` "<h1></h1><h2>Unknown Artist</h2>"
  context "when no information is given" $
    it "should generate empty tags" $
      renderText (heading "" "") `shouldBe` "<h1></h1><h2></h2>"


metadataSpec :: Spec
metadataSpec = describe "'metadata' -> Tests the function that generates the metadata list" $ do
  context "When no data is passed" $
    it "shouldn't generate any output" $
      renderText (metadata []) `shouldBe` "<ul id=\"metadata\"></ul>"
  context "When one element is passed" $
    it "should output just one li element" $
      renderText (metadata [("Key", "G")]) `shouldBe` "<ul id=\"metadata\"><li>Key: G</li></ul>"
  context "When misformed data is passed" $
    it "should use the existing data to form some sort of li element with missing data" $ do
      renderText (metadata [("", "G")]) `shouldBe` "<ul id=\"metadata\"><li>: G</li></ul>"
      renderText (metadata [("Key", "")]) `shouldBe` "<ul id=\"metadata\"><li>Key: </li></ul>"
      renderText (metadata [("", "")]) `shouldBe` "<ul id=\"metadata\"><li>: </li></ul>"
  context "When much data is passed" $
    it "should create a big list" $
      renderText (metadata [("Key", "G"), ("Time", "4/4"), ("Tempo", "115")]) `shouldBe` "<ul id=\"metadata\"><li>Key: G</li><li>Time: 4/4</li><li>Tempo: 115</li></ul>"


interleaveSpec :: Spec
interleaveSpec = describe "'interleave' -> takes two functions and 'zips' them together to one" $ do
  context "When two empty lists are passed" $
    it "should ouput an empty list" $
      interleave [] [] `shouldBe` []
  context "When one list is longer than the other" $
    it "should just append the rest of that list to the end of the output" $ do
      interleave [1,3,4] [2] `shouldBe` [1,2,3,4]
      interleave [1] [2,3,4] `shouldBe` [1,2,3,4]
  context "When the two lists are of equal length" $
    it "should just 'zip' them together" $
      interleave [1,3] [2,4] `shouldBe` [1,2,3,4]


chordSpec :: Spec
chordSpec = describe "'chord_' -> Tests the function responsible for creating the html chord structure" $ do
  context "When no chord is passed" $
    it "should generate an empty span tag" $
      renderText (chord_ "") `shouldBe` "<span class=\"chord\"></span>"
  context "When a simple chord is passed" $
    it "should generate a simple chord" $
      renderText (chord_ "G") `shouldBe` "<span class=\"chord\">G</span>"
  context "When a complicated chord is passed" $
    it "should generate a complicated chord" $
      renderText (chord_ "Gsus4maj7add9/B") `shouldBe` "<span class=\"chord\">Gsus4maj7add9/B</span>"
  context "When arguments are added to the chord" $
    it "should add those in the span tag" $ do
      renderText (chord_ [style_ "padding-left: 15px;"] "G") `shouldBe` "<span class=\"chord\" style=\"padding-left: 15px;\">G</span>"
      renderText (chord_ [class_ " something"] "G") `shouldBe` "<span class=\"chord something\">G</span>"


checkChordSpacingSpec :: Spec
checkChordSpacingSpec = describe "'checkChordSpacing' -> Tests the function that generates the padding needed to space out the chords so that they don't overlap" $ do
  context "When no data is passed" $
    it "should return an empty list []" $
      checkChordSpacing [] [] `shouldBe` []
  context "When a single chord is passed" $
    it "should return [0]" $ do
      checkChordSpacing [] ["D"] `shouldBe` [0]
      checkChordSpacing ["text"] ["D"] `shouldBe` [0]
      checkChordSpacing ["text", "text"] ["D"] `shouldBe` [0]
  context "When multiple chords are passed" $
    it "should calculate the right amount of padding" $ do
      checkChordSpacing ["text", "text", ""] ["D", "G", "A"] `shouldBe` [0, 0, 15]
      checkChordSpacing ["", "text", "", ""] ["D", "Bm", "Asus", "Em"] `shouldBe` [0, 0, 30, 60]


createChordSpec :: Spec
createChordSpec = describe "'createChord' -> Tests the function that determines if padding is needed and generates the chord appropriately" $ do
  context "When no data is passed" $
    it "should generate an empty String" $
      renderText (createChord 0 "") `shouldBe` ""
  context "When no padding should be added" $
    it "should generate the span tag withou any extra styling" $
      renderText (createChord 0 "Dm") `shouldBe` "<span class=\"chord\">Dm</span>"
  context "When extra padding is added" $
    it "should add a extra style tag to the html output" $
      renderText (createChord 15 "Dm") `shouldBe` "<span class=\"chord\" style=\"padding-left: 15px;\">Dm</span>"


-- I think I got all of the test cases here ...
lineSpec :: Spec
lineSpec = describe "'line' -> Tests the function that takes a list of song text pieces and chords and creates a usable html formatted line" $ do
  context "When no data is passed" $
    it "should return no data" $
      renderText (line ([] :: [Text]) ([] :: [Text])) `shouldBe` ""
  describe "When no song pieces are available" $ do
    context "When an empty chord is passed" $
      it "should generate a line break" $
        renderText (line ([] :: [Text]) [""]) `shouldBe` "<br>"
    context "When a single chord is passed" $
      it "should generate a span tag with the chord and then a line break" $
        renderText (line ([] :: [Text]) ["G"]) `shouldBe` "<span class=\"chord\">G</span><br>"
    --context "When multiple chords are passed" $
    --  it "should generate multiple span tags with the chords and end with a line break" $
    --    renderText (line ([] :: [Text]) ["C","D7","G"]) `shouldBe` "<span class=\"chord\">C</span><span class=\"chord\">D7</span><span class=\"chord\">G</span><br>"
  describe "When no chords are available" $ do
    context "When an empty string is passed" $
      it "should generate just a line break" $
        renderText (line [""] ([] :: [Text])) `shouldBe` "<br>"
    context "When a single text piece is passed" $
      it "should generate that text with a line break" $
        renderText (line ["some lyrics"] ([] :: [Text])) `shouldBe` "some lyrics<br>"
    context "When lots of text is passed" $
      it "should write out all of the lyrics and end with a line break" $
        renderText (line ["some lyrics", " and ", "some more lyrics"] ([] :: [Text])) `shouldBe` "some lyrics and some more lyrics<br>"
  describe "When both chords and lyrics are available" $ do
    context "When a 'normal' line is passed" $
      it "should generate a 'normal' line" $
        renderText (line ["some lyrics ", "and ", "some mo", "re lyrics"] ["D", "G", "A7", "D"]) `shouldBe` "some lyrics <span class=\"chord\">D</span>and <span class=\"chord\">G</span>some mo<span class=\"chord\">A7</span>re lyrics<span class=\"chord\">D</span><br>"
    context "When the line is supposed to begin with a chord" $
      it "should generate the starting with a chord" $
        renderText (line ["", "some lyrics ", "and ", "some more lyrics"] ["D", "G", "A7", "D"]) `shouldBe` "<span class=\"chord\">D</span>some lyrics <span class=\"chord\">G</span>and <span class=\"chord\">A7</span>some more lyrics<span class=\"chord\">D</span><br>"
    context "When two chords follow each other in a line" $
      it "should generate the line with two span tags following each other" $
        renderText (line ["some lyrics ", "and ", "", "some more lyrics"] ["D", "G", "A7", "D"]) `shouldBe` "some lyrics <span class=\"chord\">D</span>and <span class=\"chord\">G</span><span class=\"chord\" style=\"padding-left: 15px;\">A7</span>some more lyrics<span class=\"chord\">D</span><br>"


sectionSpec :: Spec
sectionSpec = describe "Test the function responsible for creating a whole songs section" $ do
  context "When no header is provided or empty section" $
    it "should generate an empty h3 tag, or an empty line" $
      renderText (section "" [([""], [""])]) `shouldBe` "<h3></h3><p><br></p>"
  context "When one line is in the section" $
    it "should format that line inside a p tag" $
      renderText (section "Amazing Grace" [(["Amazing grace, ", "how sweet the sound", "...", "", ""], ["G", "D", "A", "D", "Bm"])]) `shouldBe` "<h3>Amazing Grace</h3><p>Amazing grace, <span class=\"chord\">G</span>how sweet the sound<span class=\"chord\">D</span>...<span class=\"chord\">A</span><span class=\"chord\" style=\"padding-left: 15px;\">D</span><span class=\"chord\" style=\"padding-left: 15px;\">Bm</span><br></p>"
  context "When multiple lines are in the section" $
    it "should format each line see the line function and format it into the p tag" $
      renderText (section "Amazing Grace" [(["Amazing grace! ","How sweet the sound"],["G"]),(["That saved a ","wretch like me!"],["C"]),(["I once was lost, ","but now am found;"],["D"]),(["Was blind, ","but now I see."],["G"])]) `shouldBe` "<h3>Amazing Grace</h3><p>Amazing grace! <span class=\"chord\">G</span>How sweet the sound<br>That saved a <span class=\"chord\">C</span>wretch like me!<br>I once was lost, <span class=\"chord\">D</span>but now am found;<br>Was blind, <span class=\"chord\">G</span>but now I see.<br></p>"


copyrightSpec :: Spec
copyrightSpec = describe "Test the copyright function" $ do
  context "When no copyright is given" $
    it "should an empty p tag" $
      renderText (copyright "") `shouldBe` "<i id=\"copyright\"></i>"
  context "When the copyright isn't empty" $
    it "should just print the copyright" $
      renderText (copyright "Copyright some text Me, 2022, ...") `shouldBe` "<i id=\"copyright\">Copyright some text Me, 2022, ...</i>"


footerSpec :: Spec
footerSpec = describe "Test the function that creates the html footer" $ do
  context "When no home path is given" $
    it "should generate with the default path '.'" $
      renderText (footer "") `shouldBe` "<footer><hr><a href=\".\">Home</a></footer>"
  context "When a default home path is given" $
    it "should generate with the given path" $
      renderText (footer "./index.html") `shouldBe` "<footer><hr><a href=\"./index.html\">Home</a></footer>"

