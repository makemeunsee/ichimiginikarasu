{-# LANGUAGE OverloadedStrings #-}

module Kanjidic (kanjis, jlptKanjis) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.Text (Text, pack, append)
import GHC.IO.Handle.FD (stderr)
import qualified Data.ByteString.Lazy as L
import Text.XML.Expat.Tree

import Types
import XmlHelper

import Debug.Trace

kanjis :: Text -> FilePath -> IO [Kanji]
kanjis lang path = do
  kanjidicRaw <- L.readFile path
  let (kanjidic, mErr) = parse defaultParseOptions kanjidicRaw :: (NodeG [] Text Text, Maybe XMLParseError)

  let charNodes = filterDeepNodes ["character"] kanjidic

  case mErr of
    Nothing -> return $ fmap (loadKanji lang) charNodes
    Just err -> do
      TIO.hPutStrLn stderr $ "XML parse failed: " `append` (pack $ show err)
      return []

jlptKanjis :: Int -> FilePath -> IO [Char]
jlptKanjis level path = do
  kanjidicRaw <- L.readFile path
  let (kanjidic, mErr) = parse defaultParseOptions kanjidicRaw :: (NodeG [] Text Text, Maybe XMLParseError)

  let textJlpt = pack $ show level
  let withJlpt = any ((==) textJlpt . unsafeText) . filterDeepNodes ["misc","jlpt"]
  let charNodes = filter withJlpt $ filterDeepNodes ["character"] kanjidic

  case mErr of
    Nothing -> return $ fmap loadKanjiChar charNodes
    Just err -> do
      TIO.hPutStrLn stderr $ "XML parse failed: " `append` (pack $ show err)
      return []

loadKanjiChar = T.head . unsafeText . head . filterDeepNodes ["literal"]

loadKanji lang charNode = Kanji { char = char, codepoint = codepoint, radical = placeHolderRadical radical, strokes = strokes, onReadings = onReadings, kunReadings = kunReadings, meanings = meanings, similars = [], compounds = [] }
  where
    isUCS = attrFilter "cp_type" "ucs"
    cpValues = filterDeepNodes ["codepoint", "cp_value"] charNode
    codepoint = unsafeText $ head $ filter isUCS cpValues
    char = T.head $ unsafeText $ head $ filterDeepNodes ["literal"] charNode
    radValues = filterDeepNodes ["radical", "rad_value"] charNode
    isClassical = attrFilter "rad_type" "classical"
    radical = read $ T.unpack $ unsafeText $ head $ filter isClassical radValues
    strokes = read $ T.unpack $ unsafeText $ head $ filterDeepNodes ["misc", "stroke_count"] charNode
    kunReadings = fmap unsafeText $ filter (attrFilter "r_type" "ja_kun") $ filterDeepNodes ["reading_meaning", "rmgroup", "reading"] charNode
    onReadings = fmap unsafeText $ filter (attrFilter "r_type" "ja_on") $ filterDeepNodes ["reading_meaning", "rmgroup", "reading"] charNode
    meanings = fmap unsafeText $ filter (langFilter lang) $ filterDeepNodes ["reading_meaning", "rmgroup", "meaning"] charNode
    
langFilter "en" = noAttrFilter "m_lang"
langFilter str = attrFilter "m_lang" str

placeHolderRadical :: Int -> Radical
placeHolderRadical number = Radical { r_number = number, r_char = '?', r_strokes = 0, r_meaning = "???" }
