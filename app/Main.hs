module Main where

import System.Environment (getArgs)
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import Data.Char (ord)
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils (replace)
import Data.List (intersperse)
import System.Process (system)

--import Data.Set (toList, fromList)

--mkUniq :: Ord a => [a] -> [a]
--mkUniq = toList . fromList

data Radical = Radical { r_number :: Int, r_char :: Char, r_strokes :: Int, r_meaning :: String }
  deriving ( Show, Eq )

data Kanji = Kanji { char :: Char, codepoint :: String, radical :: Radical, strokes :: Int, onReadings :: [String], kunReadings :: [String], meanings :: [String] }
  deriving ( Show, Eq )

isCJK c = ord c >= 19968 && ord c <= 40879

printReadings :: [String] -> String
printReadings = concat . intersperse ", " . fmap (\s -> "\\mbox{" ++ s ++ "}")
--printReadings readings = concat . intersperse ", " . fmap (\s -> "\\mbox{" ++ s ++ "}") $ truncReadings
-- where
--   truncReadings = mkUniq $ fmap (takeWhile (/= '.')) readings

substitutions :: [(String, Kanji -> String)]
substitutions =
  [ ("___KANJI___", (: []) . char)
  , ("___STROKES___", show . strokes)
  , ("___RADICAL___", (: []) . r_char . radical)
  , ("___RADICAL_MEANING___", r_meaning . radical)
  , ("___ON_READINGS___",  printReadings . onReadings)
  , ("___KUN_READINGS___", printReadings . kunReadings)
  , ("___MEANINGS___", printReadings . meanings)
  , ("___1ST_BOX_HEIGHT___", firstBoxHeight)
  ]

applySubstitution :: Kanji -> String -> (String, Kanji -> String) -> String
applySubstitution kanji string (toReplace, extractor) = replace toReplace (extractor kanji) string

insertKanji :: String -> Kanji -> String -> String
insertKanji flashcardTemplate kanji pdfTexFilename = foldl (applySubstitution kanji) flashcardTemplate $ ("___KAKIKATA1___", kakikata1 pdfTexFilename) : ("___KAKIKATA2___", kakikata2 pdfTexFilename) : substitutions 

insertFlashcards string cards = replace "___FLASHCARDS___" cards string

firstBoxHeight kanji
  | stks > 12 = ""
  | otherwise = "0.75"
  where
    stks = strokes kanji

kakikata1 pdfTexFilename kanji
  | stks > 12 = "    \\def\\svgwidth{0.18\\cardwidth}\n \
\    \\fontsize{6}{6}\\selectfont\n \
\    \\input{" ++ pdfTexFilename ++ ".pdf_tex}"
  | otherwise = ""
  where
    stks = strokes kanji

kakikata2 pdfTexFilename kanji
  | stks > 12 = ""
  | otherwise = "  \\fcolorbox{red}{yellow}{%\n \
\  \\parbox[c][0.22\\cardheight][c]{\\cardwidth}{%\n \
\    \\def\\svgwidth{" ++ svgWidth ++ "\\cardwidth}\n \
\    \\input{" ++ pdfTexFilename ++ ".pdf_tex}\n \
\  }%\n \
\  }%"
  where
    stks = strokes kanji
    svgWidth = show $ min 0.975 $ fromIntegral stks * 0.135

simpleName s = QName s Nothing Nothing

findDeepElements :: [String] -> Element -> [Element]
findDeepElements names element = findDeepElements' names [element]
  where
    findDeepElements' (name : names) elements = findDeepElements' names $ concatMap (findElements $ simpleName name) elements
    findDeepElements' _ elements = elements

noAttrFilter attrName = (== Nothing) . findAttr (simpleName attrName)
attrFilter attrName attrValue = (== Just attrValue) . findAttr (simpleName attrName)

placeHolderRadical :: Int -> Radical
placeHolderRadical number = Radical { r_number = number, r_char = '?', r_strokes = 0, r_meaning = "???" }

langFilter "en" = noAttrFilter "m_lang"
langFilter str = attrFilter "m_lang" str

loadKanji :: String ->  Element -> Kanji
loadKanji lang kanjiEntry = Kanji { char = char, codepoint = codepoint, radical = placeHolderRadical radical, strokes = strokes, onReadings = onReadings, kunReadings = kunReadings, meanings = meanings }
  where
    isUCS = attrFilter "cp_type" "ucs"
    cpValues = findDeepElements ["codepoint", "cp_value"] kanjiEntry
    codepoint = strContent $ head $ filter isUCS cpValues
    char = head $ strContent $ head $ findDeepElements ["literal"] kanjiEntry
    radValues = findDeepElements ["radical", "rad_value"] kanjiEntry
    isClassical = attrFilter "rad_type" "classical"
    radical = read $ strContent $ head $ filter isClassical radValues
    strokes = read $ strContent $ head $ findDeepElements ["misc", "stroke_count"] kanjiEntry
    kunReadings = fmap strContent $ filter (attrFilter "r_type" "ja_kun") $ findDeepElements ["reading_meaning", "rmgroup", "reading"] kanjiEntry
    onReadings = fmap strContent $ filter (attrFilter "r_type" "ja_on") $ findDeepElements ["reading_meaning", "rmgroup", "reading"] kanjiEntry
    meanings = fmap strContent $ filter (langFilter lang) $ findDeepElements ["reading_meaning", "rmgroup", "meaning"] kanjiEntry

lineToRadical  = read

lineToParts :: String -> (Char, [Char])
lineToParts = read

notComment ('#' : _) = False
notComment _ = True

loadRadicalData radicals krad kanji = kanji { radical = rad { r_char = realRad, r_strokes = realCount, r_meaning = meaning } }
  where
    rad = radical kanji
    radNum = r_number $ radical kanji
    kanjiChar = char kanji
    (_, variants, meaning) = head $ filter (\(i,_,_) -> i == radNum) radicals
    (_,parts) = head $ filter ((== kanjiChar) . fst) krad
    (realRad, realCount) = maybe (head variants) id $ listToMaybe $ filter (\(k,_) -> elem k parts || k == kanjiChar) variants

vgFilename :: Int -> String -> String
vgFilename stks cp = "resources/kanji_vg/" ++ "0" ++ cp ++ qualifier
  where
    qualifier
      | stks > 12 = ""
      | otherwise = "_frames"

pdfGen :: Kanji -> IO String
pdfGen kanji = do
  _ <-system $ "inkscape -D -z --file=" ++ filename ++ ".svg --export-pdf=" ++ filename ++ ".pdf --export-latex"
  return filename
  where
    filename = vgFilename stks cp
    stks = strokes kanji
    cp = codepoint kanji

main :: IO ()
main = do
  template <- readFile "resources/template_tex"
  template_flashcard <- readFile "resources/template_flashcard_tex"

  kanjidic <- fmap (onlyElems . parseXML) $ readFile "resources/kanjidic2_sample.xml"

  radicals <- fmap (fmap lineToRadical . lines) $ readFile "resources/radicals_haskelled"

  krad <- fmap (fmap lineToParts . filter notComment . lines) $ readFile "resources/kradfile-u_haskelled"
 
  lang <- fmap head getArgs 
  codepoints <- fmap (filter isCJK) $ fmap (head . tail) getArgs >>= readFile

  let chars = concatMap (findElements $ simpleName "character") kanjidic
  let kanjis = fmap (loadRadicalData radicals krad . (loadKanji lang)) chars
  let relevants = filter (\k -> elem (char k) codepoints) kanjis
--  print relevants

  pdfTexs <- mapM pdfGen relevants
--  print pdfTexs
  let flashcards = concatMap (uncurry $ insertKanji template_flashcard) $ zip relevants pdfTexs
  let tex = insertFlashcards template flashcards
  writeFile "demo.tex" tex

  --mapM_ print kanjis
  print 0
