-- | This module contains all of the actual tree traversal/matching code.
{-# LANGUAGE OverloadedStrings, DoAndIfThenElse #-}
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
    process cur = if selMatches sel (current cur) then f cur else Just cur

transformMatchable :: Matchable -> (Cursor -> Maybe Cursor) -> [Node] -> [Node]
transformMatchable (Sel sel) f roots = transform sel f roots
transformMatchable (RSel sel m) f roots = fromMaybe [] $ do
  cur <- fromNodes roots
  return $ if selMatches sel (current cur)
           then transformMatchable m f roots
           else topNodes cur

selMatches :: CssSel -> Node -> Bool
selMatches (Id name) n | matchAttr ((==) name) "id" n = True
selMatches (Name name) n | matchAttr ((==) name) "name" n = True
selMatches (Class name) n | matchAttr (isInfixOf [name] . T.words) "class" n = True
selMatches (Attr key value) n | matchAttr ((==) value) key n = True
selMatches (Elem name) n | maybe False ((==) name) (tagName n) = True
selMatches Star _ = True
selMatches _ _ = False

matchAttr :: (T.Text -> Bool) -> T.Text -> Node -> Bool
matchAttr pred_ name = maybe False pred_ . getAttribute name
