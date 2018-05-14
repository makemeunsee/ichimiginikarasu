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

