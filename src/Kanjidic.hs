module Kanjidic (kanjis) where

import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import XmlHelper
import Types

kanjis :: String -> FilePath -> IO [Kanji]
kanjis lang path = do
  kanjidic <- loadKanjidic path
  let kanjiElements = concatMap (findElements $ simpleName "character") kanjidic
  return $ fmap (loadKanji lang) kanjiElements

langFilter "en" = noAttrFilter "m_lang"
langFilter str = attrFilter "m_lang" str

loadKanjidic :: FilePath -> IO [Element]
loadKanjidic = fmap (onlyElems . parseXML) . readFile

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

placeHolderRadical :: Int -> Radical
placeHolderRadical number = Radical { r_number = number, r_char = '?', r_strokes = 0, r_meaning = "???" }

