module Hquery.Transform where

import qualified Data.Text as T
import Data.List
import Text.XmlHtml
import Text.XmlHtml.Cursor

import Hquery.Selector

buildAttrMod :: AttrSel -> T.Text -> Cursor -> Cursor
buildAttrMod (AttrSel name attrMod) value cur = do
  let att = maybe "" id (getAttribute name (current cur))
  let f = case attrMod of
            Set -> setAttribute name (value)
            Remove | name == "class" -> do
              let classes = T.words value
              let without = filter ((flip notElem) classes) (T.words att)
              setAttribute name (T.intercalate "" without)
            Remove -> setAttribute name ""
            Append -> setAttribute name (T.append att value)
  modifyNode f cur

transform :: CssSel -> (Cursor -> Cursor) -> Node -> Node
transform sel f rootNode = topNode (transformR (fromNode rootNode))
  where
    transformR cur = do
      let result = process cur
      maybe result transformR (nextDF result)
    process cur = do
      let node = current cur
      let matchAttr attr pred_ = case getAttribute attr node of
                                   Just value | pred_ value -> f cur
                                   _ -> cur
      case sel of
        Id name -> matchAttr "id" ((==) name)
        Name name -> matchAttr "name" ((==) name)
        Class name -> matchAttr "class" (\x -> isInfixOf [name] (T.words x))
        Attr key value -> matchAttr key ((==) value)
        Elem name ->
          case tagName node of
            Just id_ | id_ == name -> f cur
            _ -> cur
        Star -> f cur
