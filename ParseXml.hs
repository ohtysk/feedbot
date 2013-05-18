module ParseXml (parseXml, findNodeList) where

import Text.XML.HaXml (Content(..), Element, xmlParse, Document(..))
import Text.XML.HaXml.Posn (Posn, noPos)
import Text.XML.HaXml.Xtract.Parse (xtract)

-- XPath で検索
findNodeList :: String -> Content Posn -> [Content Posn]
findNodeList pattern ct = xtract id pattern ct

-- Element を Content へ
toContent :: Element Posn -> Content Posn
toContent el = CElem el noPos

-- XMLのパース
parseXmlString :: String -> Element Posn
parseXmlString ct =
      let Document _ _ root _ = xmlParse "" ct
              in root

parseXml :: String -> Content Posn
parseXml xml = toContent . parseXmlString $ xml
