{-# LANGUAGE OverloadedStrings #-}

module Hquery.Transform where

import Data.Text
import Text.XmlHtml
import Text.XmlHtml.Cursor

import Hquery.Selector

buildAttrMod :: AttrSel -> Text -> Cursor -> Cursor
buildAttrMod (AttrSel name attrMod) value = case attrMod of
  Set -> do
    let f = setAttribute name (value)
    modifyNode f
  _ -> id

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
