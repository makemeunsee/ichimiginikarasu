{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Compounds (loadCompounds) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text, pack, append, toUpper)
import Data.List (sortOn, sort)
import GHC.IO.Handle.FD (stderr)
import qualified Data.ByteString.Lazy as L
import Text.XML.Expat.Tree
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, Maybe(Just), maybe, listToMaybe, maybeToList)
import Data.Ord (Ordering(..))
import Text.Read (readMaybe)

import Types
import XmlHelper

loadCompounds :: Bool -> Text -> FilePath -> FilePath -> IO (Kanji -> Kanji)
loadCompounds noDictFilling lang freqListPath jmdicPath = do
  jmdicRaw <- L.readFile jmdicPath
  let (jmdic, mErr) = parse defaultParseOptions jmdicRaw :: (XmlNode, Maybe XMLParseError)

  freqList <- fmap T.lines $ TIO.readFile freqListPath
  let cmap = compoundsMap lang jmdic

  let wordList = if noDictFilling then freqList else (freqList ++ M.keys cmap)

  let compounds kanji = take 6 $ sortOn prio $ catMaybes $ fmap (\w -> M.lookup w cmap) $ filter (T.any (== char kanji)) $ wordList
  let kanjiWithCompounds = \k -> k { compounds = compounds k }

  case mErr of
    Nothing -> return kanjiWithCompounds
    Just err -> do
      TIO.hPutStrLn stderr $ "XML parse failed: " `append` (pack $ show err)
      return id

compoundsMap :: Text -> XmlNode -> M.Map Text Compound
-- filter ((<= SPEC1) . prio . snd) $ 
compoundsMap lang jmdic = M.fromListWith selectLowestPrio $ concatMap (toCompounds lang) $ filterDeepNodes ["entry"] jmdic

selectLowestPrio c0@(Compound uid0 _ _ _ prio0) c1@(Compound uid1 _ _ _ prio1)
  | prio0 < prio1 = c0
  | prio0 > prio1 = c1
  | uid1 < uid0 = c1
  | otherwise = c0

toCompounds :: Text -> XmlNode -> [(Text, Compound)]
toCompounds lang entryNode = maybeToList $ toCompound lang entryNode

toCompound :: Text -> XmlNode -> Maybe (Text, Compound)
toCompound lang entryNode
  | keles == [] || translations == [] = Nothing
  | otherwise = Just $ (keb, Compound uid keb reading (head translations) priority)
  where
    uid = read $ T.unpack $ unsafeText $ head $ filterDeepNodes ["ent_seq"] entryNode
    keles = filterDeepNodes ["k_ele"] entryNode
    kele = head keles
    keb = unsafeText $ head $ filterDeepNodes ["keb"] kele 
    readings = filterDeepNodes ["r_ele"] entryNode
    reading = unsafeText $ head $ filterDeepNodes ["reb"] $ head $ sortOn re_pri readings
    translations = catMaybes $ fmap (toSense lang) $ filterDeepNodes ["sense"] entryNode
    priority = ke_pri kele

langFilter "fr" = attrFilter "xml:lang" "fre"
langFilter _ = attrFilter "xml:lang" "eng"

toSense :: Text -> XmlNode -> Maybe [Text]
toSense lang node
  | glosss == [] = Nothing
  | otherwise = Just $ fmap unsafeText glosss
  where
    glosss = filter (langFilter lang) $ filterDeepNodes ["gloss"] node

ke_pri :: XmlNode -> Priority
ke_pri = priority "ke_pri"

re_pri :: XmlNode -> Priority
re_pri = priority "re_pri"

priority :: Text -> XmlNode -> Priority
priority text node = maybe Bottom id $ listToMaybe $ sort $ fmap (unwrap . readMaybe . T.unpack . toUpper . unsafeText) $ children text node
  where
    unwrap Nothing = Bottom
    unwrap (Just p) = p