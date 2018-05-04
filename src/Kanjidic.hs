{-# LANGUAGE OverloadedStrings #-}

module Kanjidic (kanjis) where

import Text.XML.Light.Input
import Text.XML.Light.Proc
import Text.XML.Light.Types
import Data.Text (Text, unpack, pack)

import XmlHelper
import Types

kanjis :: Text -> FilePath -> IO [Kanji]
kanjis lang path = do
  kanjidic <- loadKanjidic path
  let kanjiElements = concatMap (findElements $ simpleName "character") kanjidic
  return $ fmap (loadKanji lang) kanjiElements

langFilter "en" = noAttrFilter "m_lang"
langFilter str = attrFilter "m_lang" str

loadKanjidic :: FilePath -> IO [Element]
loadKanjidic = fmap (onlyElems . parseXML) . readFile

loadKanji :: Text -> Element -> Kanji
loadKanji lang kanjiEntry = Kanji { char = char, codepoint = codepoint, radical = placeHolderRadical radical, strokes = strokes, onReadings = onReadings, kunReadings = kunReadings, meanings = meanings, similars = [('¤',"???")], compounds = [ Compound "???" "???" [] ] }
  where
    isUCS = attrFilter "cp_type" "ucs"
    cpValues = filterDeepElements ["codepoint", "cp_value"] kanjiEntry
    codepoint = pack $ strContent $ head $ filter isUCS cpValues
    char = head $ safeStrContent $ head $ filterDeepElements ["literal"] kanjiEntry
    radValues = filterDeepElements ["radical", "rad_value"] kanjiEntry
    isClassical = attrFilter "rad_type" "classical"
    radical = read $ strContent $ head $ filter isClassical radValues
    strokes = read $ strContent $ head $ filterDeepElements ["misc", "stroke_count"] kanjiEntry
    kunReadings = fmap (pack . safeStrContent) $ filter (attrFilter "r_type" "ja_kun") $ filterDeepElements ["reading_meaning", "rmgroup", "reading"] kanjiEntry
    onReadings = fmap (pack . safeStrContent) $ filter (attrFilter "r_type" "ja_on") $ filterDeepElements ["reading_meaning", "rmgroup", "reading"] kanjiEntry
    meanings = fmap (pack . safeStrContent) $ filter (langFilter $ unpack lang) $ filterDeepElements ["reading_meaning", "rmgroup", "meaning"] kanjiEntry

placeHolderRadical :: Int -> Radical
placeHolderRadical number = Radical { r_number = number, r_char = '?', r_strokes = 0, r_meaning = "???" }

