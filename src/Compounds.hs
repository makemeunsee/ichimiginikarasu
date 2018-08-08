{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Compounds (loadCompound, loadJmDic, loadVocList, loadFreqList) where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text, pack, append, toUpper, unpack)
import Data.List (sortOn, sort, groupBy, intersperse)
import GHC.IO.Handle.FD (stderr)
import qualified Data.ByteString.Lazy as L
import Text.XML.Expat.Tree
import qualified Data.Map.Lazy as M
import Data.Maybe (catMaybes, Maybe(Just), maybe, listToMaybe, maybeToList)
import Data.Ord (Ordering(..))
import Text.Read (readMaybe)

import Types
import XmlHelper

loadCompound :: M.Map Kreb Compound -> M.Map Char [Kreb] -> M.Map Text Int -> Kanji -> Kanji
loadCompound jmdic vocMap freqMap kanji = kanji { compounds = take 6 $ refinedCompounds }
  where
    k = char kanji
    limit = M.size freqMap
    kebs = M.findWithDefault [] k vocMap
    krebWithOrd kreb = (kreb, M.findWithDefault limit (keb kreb) freqMap)
    krebs = fmap fst $ sortOn snd $ fmap krebWithOrd kebs
    compounds = catMaybes $ fmap (`M.lookup` jmdic) krebs
    refinedCompounds = fmap concatRebs $ groupBy (\x y -> kanjide x == kanjide y && translations x == translations y) compounds
    concatRebs [] = error "groupBy failed us"
    concatRebs [c] = c
    concatRebs ((Compound u k r ts p):cs) = Compound u k rs ts p
      where
        rs = foldl T.append "" $ intersperse "/" $ r:(fmap reading cs)

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

loadVocList :: FilePath -> IO (M.Map Char [Kreb])
loadVocList vocListPath = do
  rawVocLines <- fmap T.lines $ TIO.readFile vocListPath
  let firstWord = T.takeWhile (/= ';')
  let secondWord = T.tail . T.dropWhile (/= ';')
  let vocList = fmap (\l -> (firstWord l, secondWord l)) rawVocLines

  let krebs = fmap (uncurry Kreb) vocList

  return $ M.fromListWith (++) [(kanji, [kreb]) | kreb <- krebs,
                                       kanji <- unpack $ keb kreb,
                                       isCJK kanji]

krebAndPrio :: XmlNode -> XmlNode -> (Kreb, Priority)
krebAndPrio kele rele = (Kreb k r, p)
  where
    k = unsafeText $ head $ deepGetChildren ["keb"] kele
    r = unsafeText $ head $ deepGetChildren ["reb"] rele
    p = max (ke_pri kele) (re_pri rele)

compound :: Text -> Kreb -> Priority -> XmlNode -> Compound
compound lang (Kreb k r) p entry = Compound uid k r translations p
  where
    uid = read $ T.unpack $ unsafeText $ head $ deepGetChildren ["ent_seq"] entry
    translations = concat $ catMaybes $ fmap (toSense lang) $ deepGetChildren ["sense"] entry

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