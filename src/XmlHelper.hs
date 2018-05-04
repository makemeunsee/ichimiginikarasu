module XmlHelper (simpleName, attrFilter, noAttrFilter, safeStrContent, filterDeepElements) where

import Text.XML.Light.Types
import Text.XML.Light.Proc

simpleName :: String -> QName
simpleName s = QName s Nothing Nothing

noAttrFilter attrName = (== Nothing) . findAttr (simpleName attrName)

attrFilter attrName attrValue = (== Just attrValue) . findAttr (simpleName attrName)

filterDeepElements :: [String] -> Element -> [Element]
filterDeepElements names element = filterDeepElements' names [element]
  where
    filterDeepElements' (name : names) elements = filterDeepElements' names $ concatMap (filterChildrenName ((== name) . qName)) elements
    filterDeepElements' _ elements = elements

safeStrContent = escapeTex . strContent
  where
    escapeTex ('%' : t) = "\\%" ++ escapeTex t
    escapeTex ('&' : t) = "\\&" ++ escapeTex t
    escapeTex ('$' : t) = "\\$" ++ escapeTex t
    escapeTex ('#' : t) = "\\#" ++ escapeTex t
    escapeTex ('_' : t) = "\\_" ++ escapeTex t
    escapeTex ('{' : t) = "\\{" ++ escapeTex t
    escapeTex ('}' : t) = "\\}" ++ escapeTex t
    escapeTex ('~' : t) = "\\textasciitilde" ++ escapeTex t
    escapeTex ('^' : t) = "\\textasciicircum" ++ escapeTex t
    escapeTex ('\\' : t) = "\\textbackslash" ++ escapeTex t
    escapeTex (h : t) = h : escapeTex t
    escapeTex [] = []

