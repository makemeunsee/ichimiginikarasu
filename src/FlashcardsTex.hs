module FlashcardsTex (generateTex) where

import Data.String.Utils (replace)
import Data.List (intersperse)
import System.Process (system)
import Types

generateTex :: Bool -> [Kanji] -> IO String
generateTex debug kanjis = do
  let count = length kanjis
  template <- readFile "resources/template.tex"
  template_flashcard <- readFile $ flashcardTemplate debug 

  pdfTexs <- mapM pdfGen kanjis
--  print pdfTexs
  let footer = "%unique flashcards generated: " ++ show count
  let filler 0 = 0
  let filler f = concat $ replicate (10 - f) "\\begin{flashcard}{}\\end{flashcard}"
  let flashcards = concatMap (uncurry $ insertKanji template_flashcard) $ zip kanjis pdfTexs
  return $ insertFlashcards template $ flashcards ++ "\n" ++ (filler $ count `mod` 10) ++ "\n" ++ footer

flashcardTemplate False = "resources/template_flashcard.tex"
flashcardTemplate True = "resources/template_flashcard_debug.tex"

vgFilename :: Int -> String -> String
vgFilename stks cp = "resources/kanji_vg/" ++ "0" ++ cp ++ qualifier
  where
    qualifier
      | stks > 12 = ""
      | otherwise = "_frames"

pdfGen :: Kanji -> IO String
pdfGen kanji = do
  _ <- system $ "inkscape -D -z --file=" ++ filename ++ ".svg --export-pdf=" ++ filename ++ ".pdf --export-latex"
  return filename
  where
    filename = vgFilename stks cp
    stks = strokes kanji
    cp = codepoint kanji

printMeanings :: [String] -> String
printMeanings = concat . intersperse ", "
printReadings :: [String] -> String
printReadings = printMeanings . fmap (\s -> "\\mbox{" ++ s ++ "}")
--printReadings readings = concat . intersperse ", " . fmap (\s -> "\\mbox{" ++ s ++ "}") $ truncReadings
-- where
--   truncReadings = mkUniq $ fmap (takeWhile (/= '.')) readings

suffixIfNotEmpty _ "" = ""
suffixIfNotEmpty suff str = str ++ suff

substitutions :: [(String, Kanji -> String)]
substitutions =
  [ ("___KANJI___", (: []) . char)
  , ("___STROKES___", show . strokes)
  , ("___RADICAL___", (: []) . r_char . radical)
  , ("___RADICAL_MEANING___", r_meaning . radical)
  , ("___ON_READINGS___",  suffixIfNotEmpty " \\\\[2pt]" . printReadings . onReadings)
  , ("___KUN_READINGS___", suffixIfNotEmpty " \\\\[2pt]" . printReadings . kunReadings)
  , ("___MEANINGS___", printMeanings . meanings)
  , ("___BOXES_HEIGHT___", boxesHeight)
  , ("___SIMILAR_KANJIS___", similarSubst . similars)
  , ("___COMPOUNDS___", makeCompounds fst)
  , ("___COMPOUND_TRANSLATIONS___", makeCompounds snd)
  ]

makeCompounds which = concat . intersperse "\n" . fmap ("      \\item " ++) . fmap which . take 6 . withFallback . compounds
  where
    withFallback [] = [("¤","¤")]
    withFallback l = l

applySubstitution :: Kanji -> String -> (String, Kanji -> String) -> String
applySubstitution kanji string (toReplace, extractor) = replace toReplace (extractor kanji) string

insertKanji :: String -> Kanji -> String -> String
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
\   \\input{" ++ pdfTexFilename ++ ".pdf_tex}"
  | otherwise = ""
  where
    stks = strokes kanji

kakikata2 pdfTexFilename kanji
  | stks > 12 = ""
  | otherwise = "  \\\\%\n \
\ \\centering \\parbox[c][0.2\\cardheight][c]{" ++ svgWidth ++ "\\cardinnerwidth}{%\n \
\   \\def\\svgwidth{" ++ svgWidth ++ "\\cardinnerwidth}\n \
\   \\input{" ++ pdfTexFilename ++ ".pdf_tex}\n \
\ }%"
  where
    stks = strokes kanji
    svgWidth = show $ min 0.975 $ fromIntegral stks * 0.135

similarSubst sims = "    \\begin{TAB}(e,1cm,1cm){|c|}{|" ++ pattern ++ "|}\n" ++ boxes ++ "    \\end{TAB}%"
  where
    l = length sims
    pattern = intersperse '|' $ fmap (const 'c') [0..l-1]
    boxes = concatMap toBox sims
    toBox (char, meaning) = "      \\parbox[c][1cm][c]{1cm}{%\n \
\       \\centering\n \
\       \\fontsize{22}{23}\\selectfont " ++ (char : "") ++ " \\\\\n \
\       \\fontsize{5}{5}\\selectfont \\hspace{0pt}" ++ meaning ++ " \n \
\     } \\\\\n"

