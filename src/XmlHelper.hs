module XmlHelper (simpleName, attrFilter, noAttrFilter, findDeepElements) where

import Text.XML.Light.Types
import Text.XML.Light.Proc

simpleName :: String -> QName
simpleName s = QName s Nothing Nothing

noAttrFilter attrName = (== Nothing) . findAttr (simpleName attrName)

attrFilter attrName attrValue = (== Just attrValue) . findAttr (simpleName attrName)

findDeepElements :: [String] -> Element -> [Element]
findDeepElements names element = findDeepElements' names [element]
  where
    findDeepElements' (name : names) elements = findDeepElements' names $ concatMap (findElements $ simpleName name) elements
    findDeepElements' _ elements = elements

