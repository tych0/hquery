{-# LANGUAGE OverloadedStrings #-}

module Hquery.Transform where

import Data.Text
import Text.XmlHtml
import Text.XmlHtml.Cursor

import Hquery
import Hquery.Selector

buildAttrMod :: AttrSel -> Text -> Cursor -> Cursor
buildAttrMod (AttrSel name attrMod) value = case attrMod of
  Set -> do
    let f = setAttribute name (value)
    modifyNode f
  _ -> id

buildCursorMod :: DomTransformer -> Cursor -> Cursor
buildCursorMod t = case t of
  StringXform sel attr target -> do
    maybe
      (setNode (TextNode (pack target)))
      (\s -> buildAttrMod s (pack target))
      attr
  ListStringXform sel attr targets -> id
  InvalidXform _ -> id -- exception or something?

transform :: CssSel -> (Cursor -> Cursor) -> Node -> Node
transform sel f rootNode = topNode (transformR (fromNode rootNode))
  where
    transformR cur = do
      let result = process cur
      maybe cur transformR (nextDF result)
    process cur = case sel of
      Id name -> do
        let node = current cur
        case getAttribute "id" node of
          Just id_ | id_ == name -> f cur
          _ -> cur
      _ -> cur
