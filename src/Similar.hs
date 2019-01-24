{-# LANGUAGE OverloadedStrings #-}

module Similar (loadSimilarKanjis) where

import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

import Types

loadSimilarKanjis :: [Kanji] -> FilePath -> IO (Kanji -> Kanji)
loadSimilarKanjis kanjis path = do
 similars <- fmap (take 4 . filter isCJK) . lines <$> readFile path
 return $ loadSimilars kanjis similars

loadSimilars kanjis similars kanji = kanji { similars = sims }
  where
    sims = maybe [] (tail . fmap findMeaning) $ listToMaybe $ filter ((== kanjiChar) . head) similars
    kanjiChar = char kanji
    findMeaning c = (c, maybe "???" (head . meanings) $ find (\k -> char k == c) kanjis)
