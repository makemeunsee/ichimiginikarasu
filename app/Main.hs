{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Set (toList, fromList)
import Data.Text (Text, pack)
import Data.Text.IO (hPutStrLn)
import GHC.IO.Handle.FD (stdout)

import Kanjidic
import XmlHelper
import Types
import Radicals
import Similar
import Compounds
import FlashcardsTex

mkUniq :: Ord a => [a] -> [a]
mkUniq = toList . fromList

data Params = Params
  { debug :: Bool
  , inputFile :: FilePath
  , lang :: String 
  , kanjidicPath :: FilePath
  , jmdicPath :: FilePath
  , wordsPath :: FilePath
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
     ( long "lang"
    <> short 'l'
    <> showDefault
    <> value "en"
    <> help "the language used in translations" )
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
     ( long "freqlist"
    <> short 'f'
    <> showDefault
    <> value "resources/jpn_words"
    <> help "the path to the text file (one word / line) used to select compounds from" )

main :: IO ()
main = generateFlashcards =<< execParser opts
  where
    opts = info (params <**> helper)
      ( fullDesc
     <> progDesc "Generate Latex kanji flashcards for all kanjis in file FILENAME"
     <> header "一右二烏 - a Kanji flashcards generation tool" ) 

generateFlashcards (Params debug input lang kanjidic jmdic freqlist) = do
  codepoints <- fmap (mkUniq . filter isCJK) $ readFile input
  rawKanjis <- kanjis (pack lang) kanjidic
  loadRadical <- loadRadicalData "resources/radicals_haskelled" "resources/kradfile-u_haskelled"
  loadSimilar <- loadSimilarKanjis rawKanjis "resources/jyouyou__strokeEditDistance.csv"
  loadCompound <- loadCompounds freqlist jmdic

  let kanjis = fmap (loadCompound . loadSimilar . loadRadical) rawKanjis

  let relevants = filter (\k -> elem (char k) codepoints) kanjis
  let count = length relevants
--  print relevants
  
  texContent <- generateTex debug relevants
  hPutStrLn stdout texContent
