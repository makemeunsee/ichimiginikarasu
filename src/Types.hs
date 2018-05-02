module Types (Kanji(..), Radical(..)) where

data Radical = Radical { r_number :: Int, r_char :: Char, r_strokes :: Int, r_meaning :: String }
  deriving ( Show, Eq )

data Kanji = Kanji { char :: Char, codepoint :: String, radical :: Radical, strokes :: Int, onReadings :: [String], kunReadings :: [String], meanings :: [String], similars :: [(Char, String)], compounds :: [(String, String)] }
  deriving ( Show, Eq )

