{-# LANGUAGE OverloadedStrings #-}

module Hquery.Transform where

import qualified Data.Text as T
import Data.List
import Text.XmlHtml
import Text.XmlHtml.Cursor

import Hquery.Selector

buildAttrMod :: AttrSel -> T.Text -> Cursor -> Cursor
buildAttrMod (AttrSel name attrMod) value cur =
  let f = case attrMod of
            Set -> setAttribute name (value)
            Remove -> setAttribute name ""
            Append -> do
              let a = maybe "" id (getAttribute name (current cur))
              setAttribute name (T.append a value)
  in modifyNode f cur

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
      Name name -> do
        let node = current cur
        case getAttribute "name" node of
          Just id_ | id_ == name -> f cur
          _ -> cur
      Class name -> do
        let node = current cur
        case getAttribute "class" node of
          Just classes | isInfixOf [name] (T.words classes) -> f cur
          _ -> cur
      Attr key value -> do
        let node = current cur
        case getAttribute key node of
          Just id_ | id_ == value -> f cur
          _ -> cur
      Elem name -> do
        let node = current cur
        case tagName node of
          Just id_ | id_ == name -> f cur
          _ -> cur
      Star -> f cur
