{-# LANGUAGE OverloadedStrings #-}

module FlashcardsTex (generateTex) where

import Data.List (intersperse)
import System.Process (system)
import Data.Text (Text,pack,unpack,append,intercalate,replace)
import qualified Data.Text.IO as TIO

import Types

generateTex :: Bool -> [Kanji] -> IO Text
generateTex debug kanjis = do
  let count = length kanjis
  template <- TIO.readFile "resources/template.tex"
  template_flashcard <- TIO.readFile $ flashcardTemplate debug 

  pdfTexs <- mapM pdfGen kanjis

  let footer = "%unique flashcards generated: " `append` (pack $ show count)
  let filler 0 = ""
  let filler f = foldr append "" $ replicate (10 - f) "\\begin{flashcard}{}\\end{flashcard}"

  let flashcards = foldr append "" $ fmap (uncurry $ insertKanji template_flashcard) $ zip kanjis pdfTexs

  return $ insertFlashcards template $ flashcards `append` "\n" `append` (filler $ count `mod` 10) `append` "\n" `append` footer

flashcardTemplate False = "resources/template_flashcard.tex"
flashcardTemplate True = "resources/template_flashcard_debug.tex"

vgFilename :: Int -> Text -> Text
vgFilename stks cp = "resources/kanji_vg/" `append` "0" `append` cp `append` qualifier
  where
    qualifier
      | stks > 12 = ""
      | otherwise = "_frames"

pdfGen :: Kanji -> IO Text
pdfGen kanji = do
  let cp = codepoint kanji
  let stks = strokes kanji
  let filename = vgFilename stks cp
  _ <- system $ "inkscape -D -z --file=" ++ (unpack filename) ++ ".svg --export-pdf=" ++ (unpack filename) ++ ".pdf --export-latex"
  return filename

printMeanings :: [Text] -> Text
printMeanings = intercalate ", "
printReadings :: [Text] -> Text
printReadings = printMeanings . fmap (\s -> "\\mbox{" `append` s `append` "}")

suffixIfNotEmpty _ "" = ""
suffixIfNotEmpty suff str = str `append` suff

substitutions :: [(Text, Kanji -> Text)]
substitutions =
  [ ("___KANJI___", pack . (: []) . char)
  , ("___STROKES___", pack . show . strokes)
  , ("___RADICAL___", pack . (: []) . r_char . radical)
  , ("___RADICAL_MEANING___", r_meaning . radical)
  , ("___ON_READINGS___",  suffixIfNotEmpty " \\\\[2pt]" . printReadings . onReadings)
  , ("___KUN_READINGS___", suffixIfNotEmpty " \\\\[2pt]" . printReadings . kunReadings)
  , ("___MEANINGS___", printMeanings . meanings)
  , ("___BOXES_HEIGHT___", boxesHeight)
  , ("___SIMILAR_KANJIS___", similarSubst . similars)
  , ("___COMPOUNDS___", makeCompounds kanjide)
  , ("___COMPOUND_TRANSLATIONS___", makeCompounds reading)
  ]

makeCompounds which = intercalate "\n" . fmap ("      \\item " `append`) . fmap which . take 6 . withFallback . compounds
  where
    withFallback [] = [Compound "¤" "¤" []]
    withFallback l = l

applySubstitution :: Kanji -> Text -> (Text, Kanji -> Text) -> Text
applySubstitution kanji string (toReplace, extractor) = replace toReplace (extractor kanji) string

insertKanji :: Text -> Kanji -> Text -> Text
insertKanji flashcardTemplate kanji pdfTexFilename = foldl (applySubstitution kanji) flashcardTemplate $ ("___KAKIKATA1___", kakikata1 pdfTexFilename) : ("___KAKIKATA2___", kakikata2 pdfTexFilename) : substitutions 

insertFlashcards string cards = replace "___FLASHCARDS___" cards string

boxesHeight kanji
  | stks > 12 = ""
  | otherwise = "0.77"
  where
    stks = strokes kanji

kakikata1 pdfTexFilename kanji
  | stks > 12 = "    \\def\\svgwidth{0.15\\cardinnerwidth}\n \
\   \\fontsize{6}{6}\\selectfont\n \
\   \\input{" `append` pdfTexFilename `append` ".pdf_tex}"
  | otherwise = ""
  where
    stks = strokes kanji

kakikata2 pdfTexFilename kanji
  | stks > 12 = ""
  | otherwise = "  \\\\%\n \
\ \\centering \\parbox[c][0.2\\cardheight][c]{" `append` svgWidth `append` "\\cardinnerwidth}{%\n \
\   \\def\\svgwidth{" `append` svgWidth `append` "\\cardinnerwidth}\n \
\   \\input{" `append` pdfTexFilename `append` ".pdf_tex}\n \
\ }%"
  where
    stks = strokes kanji
    svgWidth = pack $ show $ min 0.975 $ fromIntegral stks * 0.135

similarSubst sims = "    \\begin{TAB}(e,1cm,1cm){|c|}{|" `append` pattern `append` "|}\n" `append` boxes `append` "    \\end{TAB}%"
  where
    l = length sims
    pattern = pack $ intersperse '|' $ fmap (const 'c') [0..l-1]
    boxes = foldr append "" $ fmap toBox sims
    toBox (char, meaning) = "      \\parbox[c][1cm][c]{1cm}{%\n \
\       \\centering\n \
\       \\fontsize{22}{23}\\selectfont " `append` (pack $ char : "") `append` " \\\\\n \
\       \\fontsize{5}{5}\\selectfont \\hspace{0pt}" `append` meaning `append` " \n \
\     } \\\\\n"

