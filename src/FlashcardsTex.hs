{-# LANGUAGE OverloadedStrings #-}

module FlashcardsTex (generateTex) where

import Data.List (intersperse)
import System.Process (system)
import qualified Data.Text as T
import Data.Text (Text,pack,unpack,append,intercalate,replace)
import qualified Data.Text.IO as TIO

import Types

generateTex :: Bool -> Text -> [Kanji] -> IO Text
generateTex debug deck kanjis = do
  let count = length kanjis
  template <- TIO.readFile "resources/template.tex"
  template_flashcard <- TIO.readFile $ flashcardTemplate debug 

  pdfTexs <- mapM pdfGen kanjis

  let footer = "%unique flashcards generated: " `append` (pack $ show count)
  let filler f = if f == 0 then "" else foldr append "" $ replicate (10 - f) "\\begin{flashcard}{}\\end{flashcard}"

  let flashcards = foldr append "" $ fmap (replace "___DECK___" deck . (uncurry $ insertKanji template_flashcard)) $ zip kanjis pdfTexs

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

printTexts :: (Text -> Text) -> [Text] -> Text
printTexts f = intercalate ", " . fmap f

printMeanings = printTexts escapeTex

printReadings = printTexts $ (\s -> "\\mbox{" `append` s `append` "}") . escapeTex

suffixIfNotEmpty _ "" = ""
suffixIfNotEmpty suff str = str `append` suff

substitutions :: [(Text, Kanji -> Text)]
substitutions =
  [ ("___KANJI___", pack . (: []) . char)
  , ("___STROKES___", pack . show . strokes)
  , ("___RADICAL___", pack . (: []) . r_char . radical)
  , ("___RADICAL_MEANING___", escapeTex . r_meaning . radical)
  , ("___ON_READINGS___",  suffixIfNotEmpty " \\\\[2pt]" . printReadings . onReadings)
  , ("___KUN_READINGS___", suffixIfNotEmpty " \\\\[2pt]" . printReadings . kunReadings)
  , ("___MEANINGS___", printMeanings . meanings)
  , ("___BOXES_HEIGHT___", boxesHeight)
  , ("___SIMILAR_KANJIS___", similarSubst . similars)
  , ("___COMPOUNDS___", withFixesOr "\\hspace{1pt}" compoundsPrefix compoundsSuffix . makeCompounds (escapeTex . kanjide))
  , ("___COMPOUND_TRANSLATIONS___", withFixesOr "-" compoundsReadingPrefix compoundsReadingSuffix . makeCompounds readingAndTranslations)
  ]

compoundsPrefix = "    \\begin{enumerate}[leftmargin=20pt,itemsep=1pt,parsep=2pt,topsep=2pt,partopsep=2pt,font=\\normalfont\\normalsize]\n"

compoundsSuffix = "\n    \\end{enumerate}%"

compoundsReadingPrefix = "\\begin{enumerate}[leftmargin=13pt,itemsep=1pt,parsep=2pt,topsep=2pt,partopsep=2pt,font=\\normalfont\\small]%\n"

compoundsReadingSuffix = "\n\\end{enumerate}%"

withFixesOr text _ _ "" = text
withFixesOr _ prefix suffix text = prefix `append` text `append` suffix

readingAndTranslations compound = (escapeTex $ reading compound) `append` "\\\\*\n" `append` (escapeTex $ intercalate ", " $ take 3 $ translations compound)

makeCompounds which = intercalate "\n" . fmap (("\\item " `append`) . noParen . which) . compounds

noParen :: Text -> Text
noParen = replace " ," "," . fst . T.foldr noParen'("", 0)
  where
    noParen' '(' (text, count) = (text, count+1)
    noParen' ')' (text, count) = (text, count-1)
    noParen' c (text, 0) = (T.cons c text, 0)
    noParen' _ (text, count) = (text, count)

applySubstitution :: Kanji -> Text -> (Text, Kanji -> Text) -> Text
applySubstitution kanji string (toReplace, extractor) = replace toReplace (extractor kanji) string

insertKanji :: Text -> Kanji -> Text -> Text
insertKanji flashcardTemplate kanji pdfTexFilename = foldl (applySubstitution kanji) flashcardTemplate $ ("___KAKIKATA1___", kakikata1 pdfTexFilename) : ("___KAKIKATA2___", kakikata2 pdfTexFilename) : substitutions 

insertFlashcards string cards = replace "___FLASHCARDS___" cards string

boxesHeight kanji
  | stks > 12 = ""
  | otherwise = "0.87"
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
\ \\centering \\parbox[c][0.08125\\cardinnerheight][c]{" `append` svgWidth `append` "\\cardinnerwidth}{%\n \
\   \\def\\svgwidth{" `append` svgWidth `append` "\\cardinnerwidth}\n \
\   \\input{" `append` pdfTexFilename `append` ".pdf_tex}\n \
\ }%"
  where
    stks = strokes kanji
    svgWidth = pack $ show $ fromIntegral stks * 0.08125

similarSubst sims = "    \\begin{TAB}(e,1cm,1cm){|c|}{|" `append` pattern `append` "|}\n" `append` boxes `append` "    \\end{TAB}%"
  where
    l = length sims
    pattern = pack $ intersperse '|' $ fmap (const 'c') [0..l-1]
    boxes = foldr append "" $ fmap toBox sims
    toBox (char, meaning) = "      \\parbox[c][1cm][c]{1cm}{%\n \
\       \\centering\n \
\       \\fontsize{22}{23}\\selectfont " `append` (pack $ char : "") `append` " \\\\\n \
\       \\fontsize{5}{5}\\selectfont \\hspace{0pt}" `append` (escapeTex meaning) `append` " \n \
\     } \\\\\n"

escapeTex :: Text -> Text
escapeTex text = foldr (\(from, to) t -> replace from to t) text escapes

escapes :: [(Text, Text)]
escapes =
  [ ("%", "\\%")
  , ("&", "\\&")
  , ("$", "\\$")
  , ("#", "\\#")
  , ("_", "\\_")
  , ("{", "\\{")
  , ("}", "\\}")
  , ("~", "\\textasciitilde")
  , ("^", "\\textasciicircum")
  , ("\\", "\\textbackslash")
  ]

