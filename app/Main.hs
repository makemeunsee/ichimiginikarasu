module Main where

import System.Environment (getArgs)
import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import Data.Char (ord)
import Data.Maybe (fromJust, listToMaybe)
import Data.String.Utils (replace)
import Data.List (intersperse, find)
import System.Process (system)
import Options.Applicative
import Data.Semigroup ((<>))
import Data.Set (toList, fromList)

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

data Radical = Radical { r_number :: Int, r_char :: Char, r_strokes :: Int, r_meaning :: String }
  deriving ( Show, Eq )

data Kanji = Kanji { char :: Char, codepoint :: String, radical :: Radical, strokes :: Int, onReadings :: [String], kunReadings :: [String], meanings :: [String], similars :: [(Char, String)], compounds :: [(String, String)] }
  deriving ( Show, Eq )

isCJK c = ord c >= 19968 && ord c <= 40879

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
  , ("___ON_READINGS___",  suffixIfNotEmpty " \\\\[4pt]" . printReadings . onReadings)
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
  | stks > 12 = "    \\def\\svgwidth{0.18\\cardinnerwidth}\n \
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

safeStrContent = escapeTex . strContent
  where
    escapeTex ('%' : t) = "\\%" ++ escapeTex t
    escapeTex ('&' : t) = "\\&" ++ escapeTex t
    escapeTex ('$' : t) = "\\$" ++ escapeTex t
    escapeTex ('#' : t) = "\\#" ++ escapeTex t
    escapeTex ('_' : t) = "\\_" ++ escapeTex t
    escapeTex ('{' : t) = "\\{" ++ escapeTex t
    escapeTex ('}' : t) = "\\}" ++ escapeTex t
    escapeTex ('~' : t) = "\\textasciitilde" ++ escapeTex t
    escapeTex ('^' : t) = "\\textasciicircum" ++ escapeTex t
    escapeTex ('\\' : t) = "\\textbackslash" ++ escapeTex t
    escapeTex (h : t) = h : escapeTex t
    escapeTex [] = []

loadKanji :: String -> Element -> Kanji
loadKanji lang kanjiEntry = Kanji { char = char, codepoint = codepoint, radical = placeHolderRadical radical, strokes = strokes, onReadings = onReadings, kunReadings = kunReadings, meanings = meanings, similars = [('¤',"???")], compounds = [("???","???")] }
  where
    isUCS = attrFilter "cp_type" "ucs"
    cpValues = findDeepElements ["codepoint", "cp_value"] kanjiEntry
    codepoint = strContent $ head $ filter isUCS cpValues
    char = head $ safeStrContent $ head $ findDeepElements ["literal"] kanjiEntry
    radValues = findDeepElements ["radical", "rad_value"] kanjiEntry
    isClassical = attrFilter "rad_type" "classical"
    radical = read $ strContent $ head $ filter isClassical radValues
    strokes = read $ strContent $ head $ findDeepElements ["misc", "stroke_count"] kanjiEntry
    kunReadings = fmap safeStrContent $ filter (attrFilter "r_type" "ja_kun") $ findDeepElements ["reading_meaning", "rmgroup", "reading"] kanjiEntry
    onReadings = fmap safeStrContent $ filter (attrFilter "r_type" "ja_on") $ findDeepElements ["reading_meaning", "rmgroup", "reading"] kanjiEntry
    meanings = fmap safeStrContent $ filter (langFilter lang) $ findDeepElements ["reading_meaning", "rmgroup", "meaning"] kanjiEntry

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

loadSimilars kanjis similars kanji = kanji { similars = sims }
  where
    sims = maybe [('¤',"???")] (tail . fmap findMeaning) $ listToMaybe $ filter ((== kanjiChar) . head) similars
    kanjiChar = char kanji
    findMeaning c = (c, maybe "???" (head . meanings) $ find (\k -> char k == c) kanjis)

loadCompounds wordList jmdic kanji = kanji { compounds = compounds }
  where
    compounds = fmap (\compound -> (compound,"…")) $ take 6 $ filter (elem $ char kanji) wordList 

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

flashcardTemplate True = flashcardTemplate False ++ "_debug"
flashcardTemplate False = "resources/template_flashcard_tex"

generateFlashcards (Params debug input lang) = do
  template <- readFile "resources/template_tex"
  template_flashcard <- readFile $ flashcardTemplate debug 

  kanjidic <- fmap (onlyElems . parseXML) $ readFile "resources/kanjidic2.xml"
  jmdic <- fmap (onlyElems . parseXML) $ readFile "resources/JMdict"

  radicals <- fmap (fmap lineToRadical . lines) $ readFile "resources/radicals_haskelled"

  krad <- fmap (fmap lineToParts . filter notComment . lines) $ readFile "resources/kradfile-u_haskelled"
  
  similars <- fmap (fmap (take 4) . fmap (filter isCJK) . lines) $ readFile "resources/jyouyou__strokeEditDistance.csv"

  wordList <- fmap lines $ readFile "resources/jpn_words"

  codepoints <- fmap (mkUniq . filter isCJK) $ readFile input

  let kanjisNoRad = concatMap (findElements $ simpleName "character") kanjidic
  let kanjisNoSims = fmap (loadRadicalData radicals krad . loadKanji lang) kanjisNoRad
  let kanjisNoCompounds = fmap (loadSimilars kanjisNoSims similars) kanjisNoSims
  let kanjis = fmap (loadCompounds wordList jmdic) kanjisNoCompounds

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
