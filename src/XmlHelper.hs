module XmlHelper (XmlNode, deepAssertions, children, deepGetChildren, unsafeText, attrFilter, noAttrFilter, debugHead) where

import Text.XML.Expat.Tree
import Text.XML.Expat.Proc
import Data.Text (Text)

type XmlNode = NodeG [] Text Text

children :: Text -> XmlNode -> [XmlNode]
children name = childFilter ((== name) . getName)

-- find all nested nodes matching the 'address' (tag hierarchy)
deepGetChildren :: [Text] -> XmlNode -> [XmlNode]
deepGetChildren address node = deepGetChildren' address [node]

deepGetChildren' :: [Text] -> [XmlNode] -> [XmlNode]
deepGetChildren' (name : names) nodes = deepGetChildren' names $ concatMap (children name) nodes
deepGetChildren' _ nodes = nodes

unsafeText :: XmlNode -> Text
unsafeText n = (getText . debugHead (show n) . filter isText . getChildren) n

noAttrFilter :: Text -> XmlNode -> Bool
noAttrFilter attrName = all ((/= attrName) . fst) . getAttributes

attrFilter :: Text -> Text -> XmlNode -> Bool
attrFilter attrName attrValue = elem (attrName, attrValue) . getAttributes

childFilter :: (XmlNode -> Bool) -> XmlNode -> [XmlNode]
childFilter childPredicate node = filter childPredicate $ getChildren node

-- assert node has at least a child matching each (address, text) requirement of the list
deepAssertions :: [([Text], Text)] -> XmlNode -> Bool
deepAssertions [] _ = True
deepAssertions ((address, value):rs) node = truth1 && truth2 
  where
    truth1 = any (hasText value) $ deepGetChildren address node
    truth2 = deepAssertions rs node

hasText :: Text -> XmlNode -> Bool
hasText text = elem text . fmap getText . filter isText . getChildren

debugHead :: String -> [a] -> a
debugHead text [] = error text
debugHead _ l = head l
