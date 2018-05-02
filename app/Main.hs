module Main where

import Data.String.Utils (replace)
import Data.List (intersperse)
import System.Process (system)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Set (toList, fromList)

import Kanjidic
import XmlHelper
import Types
import Radicals
import Similar
import Compounds

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

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

data Params = Params
  { debug :: Bool
  , inputFile :: String
  , lang :: String }

params :: Parser Params
params = Params
  <$> switch
     ( long "debug"
    <> short 'd'
    <> help "draw frame boxes to help debug generated latex code" )
  <*> strOption
     ( long "input"
    <> short 'i'
    <> metavar "FILENAME"
    <> help "text file containing the kanjis of which to create flashcards" )
  <*> strOption
     ( long "lang"
    <> short 'l'
    <> showDefault
    <> value "en"
    <> help "the language used in translations" )

main :: IO ()
main = generateFlashcards =<< execParser opts
  where
    opts = info (params <**> helper)
      ( fullDesc
     <> progDesc "Generate Latex kanji flashcards for all kanjis in file FILENAME"
     <> header "一右二烏 - a Kanji flashcards generation tool" ) 

flashcardTemplate False = "resources/template_flashcard.tex"
flashcardTemplate True = "resources/template_flashcard_debug.tex"

generateFlashcards (Params debug input lang) = do
  template <- readFile "resources/template.tex"
  template_flashcard <- readFile $ flashcardTemplate debug 

  codepoints <- fmap (mkUniq . filter isCJK) $ readFile input

  rawKanjis <- kanjis lang "resources/kanjidic2.xml"
  loadRadical <- loadRadicalData "resources/radicals_haskelled" "resources/kradfile-u_haskelled"
  loadSimilar <- loadSimilarKanjis rawKanjis "resources/jyouyou__strokeEditDistance.csv"
  loadCompound <- loadCompounds "resources/jpn_words" "resources/JMdict"

  let kanjis = fmap (loadCompound . loadSimilar . loadRadical) rawKanjis

-- fmap (loadRadicalData radicals krad . (loadKanji lang)) chars
  let relevants = filter (\k -> elem (char k) codepoints) kanjis
  let count = length relevants
--  print relevants

  pdfTexs <- mapM pdfGen relevants
--  print pdfTexs
  let footer = "%unique flashcards generated: " ++ show count
  let filler 0 = 0
  let filler f = concat $ replicate (10 - f) "\\begin{flashcard}{}\\end{flashcard}"
  let flashcards = concatMap (uncurry $ insertKanji template_flashcard) $ zip relevants pdfTexs
  let tex = insertFlashcards template $ flashcards ++ "\n" ++ (filler $ count `mod` 10) ++ "\n" ++ footer
  --writeFile "demo.tex" tex

  --mapM_ print kanjis
  putStrLn tex
