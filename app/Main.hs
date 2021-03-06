{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn)
import GHC.IO.Handle.FD (stdout)
import Data.Maybe (listToMaybe)

import Kanjidic
import XmlHelper
import Types
import Radicals
import Similar
import Compounds
import FlashcardsTex

import Debug.Trace (traceShow, traceShowId)

mkUniq :: Ord a => [a] -> [a]
mkUniq = Set.toList . Set.fromList

data Params = Params
  { debug :: Bool
  , inputFile :: FilePath
  , deck :: String 
  , lang :: String 
  , kanjidicPath :: FilePath
  , jmdicPath :: FilePath
  , wordsPath :: FilePath
  , freqsPath :: FilePath
  , noDictFilling :: Bool
  }

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
     ( long "deck"
    <> short 'n'
    <> showDefault
    <> value ""
    <> help "the name of the card deck to generate" )
  <*> strOption
     ( long "lang"
    <> short 'l'
    <> showDefault
    <> value "en"
    <> help "the language used in translations. Supported: 'en', 'fr', other values fallback to 'en'" )
  <*> strOption
     ( long "kanjidic"
    <> short 'k'
    <> showDefault
    <> value "resources/kanjidic2.xml"
    <> help "the path to the kanjidic XML file used to look up kanji details" )
  <*> strOption
     ( long "jmdic"
    <> short 'j'
    <> showDefault
    <> value "resources/JMdict"
    <> help "the path to the JMdict XML file used to look up compounds pronunciation & translations" )
  <*> strOption
     ( long "voclist"
    <> short 'v'
    <> showDefault
    <> value "resources/jpn_words_KG_all.utf8"
    <> help "the path to the text file (formatted like '乃公;だいこう') used to select compounds from" )
  <*> strOption
     ( long "freqlist"
    <> short 'f'
    <> showDefault
    <> value "resources/freq_list_Michiel_Kamermans.txt"
    <> help "the path to the text file of compounds ordered by descending frequency" )
  <*> switch
     ( long "no-dict-fill"
    <> short 'n'
    <> help "do not look for compounds in the dictionary if the vocabulary list gets exhausted" )

main :: IO ()
main = generateFlashcards =<< execParser opts
  where
    opts = info (params <**> helper)
      ( fullDesc
     <> progDesc "Generate Latex kanji flashcards for all kanjis in file FILENAME"
     <> header "一右二烏 - a Kanji flashcards generation tool")

radicalFilePath lang
  | lang == "fr" = "resources/radicals_haskelled_fr"
  | otherwise = "resources/radicals_haskelled"

orderFrom :: [a] -> (a -> b) -> (b -> Maybe c) -> [c] -> [c]
orderFrom refList refKey fromKey toOrder = foldr aToC [] refList
  where
    aToC el acc = maybe acc (: acc) (fromKey $ refKey el)

generateFlashcards (Params debug input deck lang kanjidic jmdic vocList freqlist noDictFilling) = do
  codepoints <- traceShow "read code points". filter isCJK <$> readFile input
  rawKanjis <- traceShow "read kanjis" <$> kanjis (pack lang) kanjidic
  loadRadical <- traceShow "prepared radical func" <$> loadRadicalData (radicalFilePath lang) "resources/kradfile-u_haskelled"
  loadSimilar <- traceShow "prepared similarity func" <$> loadSimilarKanjis rawKanjis "resources/jyouyou__strokeEditDistance.csv"
  vocMap <- traceShow "prepared voc map" <$> loadVocList vocList
  jmdic <- traceShow "prepared compounds map" <$> loadJmDic (pack lang) jmdic
  freqMap <- traceShow "prepared freq map" <$> loadFreqList freqlist

  let kanjis = traceShow "loaded kanjis" $ fmap (loadCompound jmdic vocMap freqMap . loadSimilar . loadRadical) rawKanjis

  let relevants = traceShow "filtered relevant kanjis" $ filter (\k -> elem (char k) $ mkUniq codepoints) kanjis
  let reordered = traceShow "reordered" $ fmap (\(i, k) -> k { customOrd = i }) $ zip [0..] $ orderFrom codepoints id (\c -> listToMaybe $ filter ((== c) . char) relevants) relevants

  texContent <- traceShow "generated tex content" <$> generateTex debug (pack deck) reordered
  hPutStrLn stdout texContent
