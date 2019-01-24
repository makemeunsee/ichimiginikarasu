module Types (Kanji(..), Radical(..), Compound(..), Priority(..), Kreb(..), isCJK) where

import Data.Text (Text)
import Data.Char (ord)

isCJK c = ord c >= 19968 && ord c <= 40879

data Radical = Radical { r_number :: Int, r_char :: Char, r_strokes :: Int, r_meaning :: Text }
  deriving ( Show, Eq )

data Kanji = Kanji { customOrd :: Int, char :: Char, codepoint :: Text, radical :: Radical, strokes :: Int, onReadings :: [Text], kunReadings :: [Text], meanings :: [Text], similars :: [(Char, Text)], compounds :: [Compound] }
  deriving ( Show, Eq )

data Compound = Compound { uid :: Int, kanjide :: Text, reading :: Text, translations :: [Text], prio :: Priority }
  deriving ( Show, Read, Eq )
    
data Priority = NF01 | NF02 | NF03 | NF04 | NF05 | NF06 | NF07 | NF08 | NF09 | NF10 |
                NF11 | NF12 | NF13 | NF14 | NF15 | NF16 | NF17 | NF18 | NF19 | NF20 |
                ICHI1 | NEWS1 | SPEC1 |
                NF21 | NF22 | NF23 | NF24 | NF25 | NF26 | NF27 | NF28 | NF29 | NF30 |
                NF31 | NF32 | NF33 | NF34 | NF35 | NF36 | NF37 | NF38 | NF39 | NF40 |
                NEWS2 | SPEC2 |
                Bottom deriving (Read, Eq, Ord, Show)

data Kreb = Kreb { keb :: Text, reb :: Text } deriving (Show, Eq, Ord)
