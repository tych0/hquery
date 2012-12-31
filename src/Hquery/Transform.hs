{-# LANGUAGE OverloadedStrings #-}

module Hquery.Transform where

import Data.Text
import Text.XmlHtml
import Text.XmlHtml.Cursor

import Hquery
import Hquery.Selector

buildAttrMod :: AttrSel -> Text -> Cursor -> Cursor
buildAttrMod attrSel value = case attrSel of
  AttrSel name Nothing -> do
    let f = setAttribute name (value)
    modifyNode f
  _ -> id

buildCursorMod :: DomTransformer -> Cursor -> Cursor
buildCursorMod t = case t of
  StringXform sel target -> do
    let attrSel = extractAttrSel sel
    maybe
      (setNode (TextNode (pack target)))
      (\s -> buildAttrMod s (pack target))
      attrSel
  ListStringXform sel targets -> id
  InvalidXform _ -> id -- exception or something?

transform :: CssSel -> (Cursor -> Cursor) -> Node -> Node
transform sel f rootNode = topNode (transformR (fromNode rootNode))
  where
    transformR cur = do
      let result = process cur
      maybe cur transformR (nextDF result)
    process cur = case sel of
      Id name _ -> do
        let node = current cur
        case getAttribute "id" node of
          Just id_ | id_ == name -> f cur
          _ -> cur
      _ -> cur
