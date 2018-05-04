{-# LANGUAGE OverloadedStrings #-}

module Compounds (loadCompounds) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text, pack, append)
import GHC.IO.Handle.FD (stderr)
import qualified Data.ByteString.Lazy as L
import Text.XML.Expat.Tree

import Types
import XmlHelper

loadCompounds :: FilePath -> FilePath -> IO (Kanji -> Kanji)
loadCompounds freqListPath jmdicPath = do
  jmdicRaw <- L.readFile jmdicPath
  let (jmdic, mErr) = parse defaultParseOptions jmdicRaw :: (NodeG [] Text Text, Maybe XMLParseError)

  wordList <- fmap T.lines $ TIO.readFile freqListPath

  case mErr of
    Nothing -> return $ loadCompounds' (wordList ++ kebs jmdic)
    Just err -> do
      TIO.hPutStrLn stderr $ "XML parse failed: " `append` (pack $ show err)
      return id

kebs :: NodeG [] Text Text -> [Text]
kebs jmdic = fmap getText $ concatMap ((filter isText) . getChildren) $ filterDeepNodes ["entry","k_ele","keb"] jmdic

loadCompounds' wordList kanji = kanji { compounds = compounds }
  where
    compounds = fmap (\compound -> Compound compound "…" []) $ take 6 $ filter (T.any (== char kanji)) wordList

