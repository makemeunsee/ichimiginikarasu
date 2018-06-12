module XmlHelper (XmlNode, children, filterDeepNodes, unsafeText, attrFilter, noAttrFilter) where

import Text.XML.Expat.Tree
import Text.XML.Expat.Proc
import Data.Text (Text)

type XmlNode = NodeG [] Text Text

children :: Text -> XmlNode -> [XmlNode]
children name node = filter ((== name) . getName) $ getChildren node

filterDeepNodes :: [Text] -> XmlNode -> [XmlNode]
filterDeepNodes names node = filterDeepNodes' names [node]

filterDeepNodes' :: [Text] -> [XmlNode] -> [XmlNode]
filterDeepNodes' (name : names) nodes = filterDeepNodes' names $ concatMap (children name) nodes
filterDeepNodes' _ nodes = nodes

unsafeText :: XmlNode -> Text
unsafeText = getText . head . filter isText . getChildren 

noAttrFilter :: Text -> XmlNode -> Bool
noAttrFilter attrName = all ((/= attrName) . fst) . getAttributes

attrFilter :: Text -> Text -> XmlNode -> Bool
attrFilter attrName attrValue = any (== (attrName, attrValue)) . getAttributes

