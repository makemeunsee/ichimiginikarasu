{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Compounds (loadCompound, loadJmDic, loadVocList, loadFreqList) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text, pack, append, toUpper, unpack)
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

import Debug.Trace (traceShowId, traceShow)

loadCompound :: M.Map Kreb Compound -> M.Map Char [(Kreb, Int)] -> M.Map Text Int -> Kanji -> Kanji
loadCompound jmdic vocMap freqMap kanji = kanji { compounds = compounds }
  where
    k = char kanji
    krebs = fmap fst $ sortOn snd $ fmap (\(kreb, _) -> (kreb, M.findWithDefault 1000000 (keb kreb) freqMap)) $ M.findWithDefault [] k vocMap
    compounds = take 6 $ catMaybes $ fmap (\kreb -> M.lookup kreb jmdic) krebs

loadFreqList :: FilePath -> IO (M.Map Text Int)
loadFreqList freqListPath = do
  compounds <- fmap T.lines $ TIO.readFile freqListPath
  return $ M.fromList $ zip compounds [0..]

loadJmDic :: Text -> FilePath -> IO (M.Map Kreb Compound)
loadJmDic lang jmdicPath = do
  jmdicRaw <- L.readFile jmdicPath
  let (jmdic, mErr) = parse defaultParseOptions jmdicRaw :: (XmlNode, Maybe XMLParseError)
  let jmdicEntries = deepGetChildren ["entry"] jmdic

  let jmdic = M.fromList [(key, compound lang key priority entry) | entry <- jmdicEntries,
                                                                    kele <- deepGetChildren ["k_ele"] entry,
                                                                    rele <- deepGetChildren ["r_ele"] entry,
                                                                    let (key, priority) = krebAndPrio kele rele,
                                                                    priority < Bottom]
  case mErr of
    Nothing -> return jmdic
    Just err -> do
                  TIO.hPutStrLn stderr $ "XML parse failed: " `append` (pack $ show err)
                  return M.empty

loadVocList :: FilePath -> IO (M.Map Char [(Kreb, Int)])
loadVocList vocListPath = do
  rawVocLines <- fmap T.lines $ TIO.readFile vocListPath
  let firstWord = T.takeWhile (/= ';')
  let secondWord = T.tail . T.dropWhile (/= ';')
  let vocList = fmap (\l -> (firstWord l, secondWord l)) rawVocLines

  let krebs = fmap (uncurry Kreb) vocList

  return $ M.fromListWith (++) [(kanji, [(kreb, i)]) | (kreb, i) <- zip krebs [0..],
                                       kanji <- unpack $ keb kreb,
                                       isCJK kanji]

krebAndPrio :: XmlNode -> XmlNode -> (Kreb, Priority)
krebAndPrio kele rele = (Kreb k r, p)
  where
    k = unsafeText $ head $ deepGetChildren ["keb"] kele
    r = unsafeText $ head $ deepGetChildren ["reb"] rele
    p = max (ke_pri kele) (re_pri rele)

compound :: Text -> Kreb -> Priority -> XmlNode -> Compound
compound lang (Kreb k r) p entry = Compound uid k r (head translations) p
  where
    uid = read $ T.unpack $ unsafeText $ head $ deepGetChildren ["ent_seq"] entry
    translations = catMaybes $ fmap (toSense lang) $ deepGetChildren ["sense"] entry

-- loadCompounds :: Bool -> Text -> FilePath -> FilePath -> IO (Kanji -> Kanji)
-- loadCompounds noDictFilling lang vocListPath jmdicPath = do
--   jmdicRaw <- L.readFile jmdicPath
--   let (jmdic, mErr) = parse defaultParseOptions jmdicRaw :: (XmlNode, Maybe XMLParseError)
--   let jmdicEntries = deepGetChildren ["entry"] jmdic

--   rawVocLines <- fmap T.lines $ TIO.readFile vocListPath
--   let firstWord = T.takeWhile (/= ';')
--   let secondWord = T.tail . T.dropWhile (/= ';')
--   let vocList = fmap (\l -> (firstWord l, secondWord l)) rawVocLines
  
--   let krebs = fmap (uncurry Kreb) vocList
  
--   let compoundsList = sortOn prio $ catMaybes $ fmap (loadCompound lang jmdicEntries) krebs

--   let compounds kanji = take 6 $ filter (T.any (== char kanji) . kanjide) $ compoundsList
--   let kanjiWithCompounds = \k -> k { compounds = compounds k }

--   case mErr of
--     Nothing -> return kanjiWithCompounds
--     Just err -> do
--       TIO.hPutStrLn stderr $ "XML parse failed: " `append` (pack $ show err)
--       return id

-- loadCompound :: Text -> [XmlNode] -> Kreb -> Maybe Compound
-- loadCompound lang entries kreb@(Kreb keb reb) = listToMaybe $ fmap (toCompound lang kreb) entryNodes
--   where
--     entryNodes = filter (deepAssertions [(["k_ele","keb"], keb), (["r_ele","reb"], reb)]) entries

-- toCompound :: Text -> Kreb -> XmlNode -> Compound
-- toCompound lang (Kreb keb reb) entryNode = Compound uid keb reb (head translations) priority
--   where
--     uid = read $ T.unpack $ unsafeText $ head $ deepGetChildren ["ent_seq"] entryNode
--     translations = catMaybes $ fmap (toSense lang) $ deepGetChildren ["sense"] entryNode
--     keles = deepGetChildren ["k_ele"] entryNode
--     kele = head $ filter (deepAssertions [(["keb"], keb)]) keles
--     reles = deepGetChildren ["r_ele"] entryNode
--     rele = head $ filter (deepAssertions [(["reb"], reb)]) reles
--     priority = min (ke_pri kele) (re_pri rele)

langFilter "fr" = attrFilter "xml:lang" "fre"
langFilter _ = attrFilter "xml:lang" "eng"

toSense :: Text -> XmlNode -> Maybe [Text]
toSense lang node
  | glosss == [] = Nothing
  | otherwise = Just $ fmap unsafeText glosss
  where
    glosss = filter (langFilter lang) $ deepGetChildren ["gloss"] node

ke_pri :: XmlNode -> Priority
ke_pri = ele_pri "ke_pri"

re_pri :: XmlNode -> Priority
re_pri = ele_pri "re_pri"

ele_pri :: Text -> XmlNode -> Priority
ele_pri text node = maybe Bottom id $ listToMaybe $ sort $ fmap (unwrap . readMaybe . T.unpack . toUpper . unsafeText) $ children text node
  where
    unwrap Nothing = Bottom
    unwrap (Just p) = p