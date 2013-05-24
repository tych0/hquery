-- | This module contains all of the actual tree traversal/matching code.
{-# LANGUAGE OverloadedStrings #-}
module Text.Hquery.Internal.Transform where

import qualified Data.Text as T
import Data.List
import Data.Maybe
import Text.XmlHtml
import Text.XmlHtml.Cursor

import Text.Hquery.Internal.Selector

buildAttrMod :: T.Text -> AttrMod -> T.Text -> Cursor -> Cursor
buildAttrMod name attrMod value cur = do
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
            AppendAttr | name == "class" -> do
              let classes = value : (T.words att)
              setAttribute name (T.unwords classes)
            AppendAttr -> setAttribute name (T.append att value)
  modifyNode f cur

transform :: CssSel -> (Cursor -> Maybe Cursor) -> [Node] -> [Node]
transform sel f roots = fromMaybe [] $ do
  cur <- fromNodes roots
  transformed <- transformR cur
  return $ topNodes transformed
  where
    transformR cur =
      let result = process cur
      in maybe result transformR $ do
         r <- result
         next <- nextDF r
         return next
    process cur = do
      let node = current cur
      let matchAttr attr pred_ = case getAttribute attr node of
                                   Just value | pred_ value -> f cur
                                   _ -> Just cur
      case sel of
        Id name -> matchAttr "id" ((==) name)
        Name name -> matchAttr "name" ((==) name)
        Class name -> matchAttr "class" (\x -> isInfixOf [name] (T.words x))
        Attr key value -> matchAttr key ((==) value)
        Elem name ->
          case tagName node of
            Just id_ | id_ == name -> f cur
            _ -> Just cur
        Star -> f cur
