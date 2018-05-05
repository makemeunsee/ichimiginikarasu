{-# LANGUAGE OverloadedStrings #-}

module Compounds (loadCompounds) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text, pack, append)
import GHC.IO.Handle.FD (stderr)
import qualified Data.ByteString.Lazy as L
import Text.XML.Expat.Tree
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes)

import Types
import XmlHelper

import Debug.Trace

loadCompounds :: Bool -> FilePath -> FilePath -> IO (Kanji -> Kanji)
loadCompounds noDictFilling freqListPath jmdicPath = do
  jmdicRaw <- L.readFile jmdicPath
  let (jmdic, mErr) = parse defaultParseOptions jmdicRaw :: (NodeG [] Text Text, Maybe XMLParseError)

  freqList <- fmap T.lines $ TIO.readFile freqListPath
  let cmap = compoundsMap jmdic

  let wordList = if noDictFilling then freqList else (freqList ++ M.keys cmap)

  let compounds kanji = take 6 $ catMaybes $ fmap (\w -> M.lookup w cmap) $ filter (T.any (== char kanji)) $ wordList
  let kanjiWithCompounds = \k -> k { compounds = compounds k }

  case mErr of
    Nothing -> return kanjiWithCompounds
    Just err -> do
      TIO.hPutStrLn stderr $ "XML parse failed: " `append` (pack $ show err)
      return id

compoundsMap :: NodeG [] Text Text -> M.Map Text Compound
compoundsMap jmdic = M.fromList $ concatMap toCompounds $ filterDeepNodes ["entry"] jmdic

toCompounds node = fmap (\k -> (k, toCompound k node)) kebs
  where
    kebs = fmap unsafeText $ filterDeepNodes ["k_ele", "keb"] node

toCompound keb node = Compound keb "?" []

