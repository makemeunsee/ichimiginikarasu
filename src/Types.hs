{-# LANGUAGE OverloadedStrings #-}

module Types (Kanji(..), Radical(..), Compound(..), isCJK) where

import Data.Text (Text)
import Data.Char (ord)

isCJK c = ord c >= 19968 && ord c <= 40879

data Radical = Radical { r_number :: Int, r_char :: Char, r_strokes :: Int, r_meaning :: Text }
  deriving ( Show, Eq )

data Kanji = Kanji { char :: Char, codepoint :: Text, radical :: Radical, strokes :: Int, onReadings :: [Text], kunReadings :: [Text], meanings :: [Text], similars :: [(Char, Text)], compounds :: [Compound] }
  deriving ( Show, Eq )

data Compound = Compound { uid :: Int, kanjide :: Text, reading :: Text, senses :: [Text] }
  deriving ( Show, Eq )
