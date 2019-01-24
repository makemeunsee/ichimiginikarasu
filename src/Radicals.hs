module Radicals (loadRadicalData) where

import Data.Maybe (listToMaybe, fromMaybe)
import Types

loadRadicalData :: FilePath -> FilePath -> IO (Kanji -> Kanji)
loadRadicalData radsPath kradPath = do
  radicals <- fmap lineToRadical . lines <$> readFile radsPath
  krad <- fmap lineToParts . filter notComment . lines <$> readFile kradPath
  return $ loadRadicalData' radicals krad

lineToRadical = read

lineToParts :: String -> (Char, String)
lineToParts = read

notComment ('#' : _) = False
notComment _ = True

loadRadicalData' radicals krad kanji = kanji { radical = rad { r_char = realRad, r_strokes = realCount, r_meaning = meaning } }
  where
    rad = radical kanji
    radNum = r_number $ radical kanji
    kanjiChar = char kanji
    (_, variants, meaning) = head $ filter (\(i,_,_) -> i == radNum) radicals
    (_,parts) = head $ filter ((== kanjiChar) . fst) krad
    (realRad, realCount) = fromMaybe (head variants) $ listToMaybe $ filter (\(k,_) -> elem k parts || k == kanjiChar) variants
