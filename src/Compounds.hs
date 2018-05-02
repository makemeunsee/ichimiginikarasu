module Compounds (loadCompounds) where

import Types

loadCompounds :: FilePath -> FilePath -> IO (Kanji -> Kanji)
loadCompounds freqListPath jmdicPath = do
  -- jmdic <- fmap (onlyElems . parseXML) $ readFile jmdicPath
  wordList <- fmap lines $ readFile freqListPath
  return $ loadCompounds' wordList

loadCompounds' wordList kanji = kanji { compounds = compounds }
  where
    compounds = fmap (\compound -> (compound,"…")) $ take 6 $ filter (elem $ char kanji) wordList
    
