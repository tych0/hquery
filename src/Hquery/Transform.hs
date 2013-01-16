module Hquery.Transform where

import qualified Data.Text as T
import Data.List
import Data.Maybe
import Text.XmlHtml
import Text.XmlHtml.Cursor

import Control.Monad

import Hquery.Error
import Hquery.Selector

buildAttrMod :: AttrSel -> T.Text -> Cursor -> Cursor
buildAttrMod (AttrSel name attrMod) value cur = do
  let att = maybe "" id (getAttribute name (current cur))
  let remove n = case n of
                 Element { elementTag = tag
                         , elementAttrs = attrs
                         , elementChildren = kids
                         }
                   -> Element { elementTag = tag
                              , elementAttrs = filter ((name /=) . fst) attrs
                              , elementChildren = kids
                              }
                 _ -> n
  let f = case attrMod of
            Set -> setAttribute name (value)
            Remove | name == "class" -> do
              let classes = T.words value
              let without = filter ((flip notElem) classes) (T.words att)
              let result = T.intercalate "" without
              if T.null result
                then remove
                else setAttribute name result
            Remove -> remove
            Append | name == "class" -> do
              let classes = value : (T.words att)
              setAttribute name (T.unwords classes)
            Append -> setAttribute name (T.append att value)
  modifyNode f cur
buildAttrMod CData _ _ = (raise "shouldn't be attr-modding a CData")

transform :: CssSel -> (Cursor -> Cursor) -> [Node] -> [Node]
transform sel f roots =
  fromMaybe [] $ liftM (\c -> topNodes (transformR c)) (fromNodes roots)
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
