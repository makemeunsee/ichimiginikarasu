module XmlHelper (children, filterDeepNodes, unsafeText, attrFilter, noAttrFilter) where

import Text.XML.Expat.Tree
import Text.XML.Expat.Proc
import Data.Text (Text)

children :: Text -> NodeG [] Text Text -> [NodeG [] Text Text]
children name node = filter ((== name) . getName) $ getChildren node

filterDeepNodes :: [Text] -> NodeG [] Text Text -> [NodeG [] Text Text]
filterDeepNodes names node = filterDeepNodes' names [node]

filterDeepNodes' :: [Text] -> [NodeG [] Text Text] -> [NodeG [] Text Text]
filterDeepNodes' (name : names) nodes = filterDeepNodes' names $ concatMap (children name) nodes
filterDeepNodes' _ nodes = nodes

unsafeText :: NodeG [] Text Text -> Text
unsafeText = getText . head . filter isText . getChildren 

noAttrFilter :: Text -> NodeG [] Text Text -> Bool
noAttrFilter attrName = all ((/= attrName) . fst) . getAttributes

attrFilter :: Text -> Text -> NodeG [] Text Text -> Bool
attrFilter attrName attrValue = any (== (attrName, attrValue)) . getAttributes

--simpleName :: String -> QName
--simpleName s = QName s Nothing Nothing

--filterDeepElements :: [String] -> Element -> [Element]
--filterDeepElements names element = filterDeepElements' names [element]
--  where
--    filterDeepElements' (name : names) elements = filterDeepElements' names $ concatMap (filterChildrenName ((== name) . qName)) elements
--    filterDeepElements' _ elements = elements
--
--safeStrContent = escapeTex . strContent
--  where
--    escapeTex ('%' : t) = "\\%" ++ escapeTex t
--    escapeTex ('&' : t) = "\\&" ++ escapeTex t
--    escapeTex ('$' : t) = "\\$" ++ escapeTex t
--    escapeTex ('#' : t) = "\\#" ++ escapeTex t
--    escapeTex ('_' : t) = "\\_" ++ escapeTex t
--    escapeTex ('{' : t) = "\\{" ++ escapeTex t
--    escapeTex ('}' : t) = "\\}" ++ escapeTex t
--    escapeTex ('~' : t) = "\\textasciitilde" ++ escapeTex t
--    escapeTex ('^' : t) = "\\textasciicircum" ++ escapeTex t
--    escapeTex ('\\' : t) = "\\textbackslash" ++ escapeTex t
--    escapeTex (h : t) = h : escapeTex t
--    escapeTex [] = []
--
