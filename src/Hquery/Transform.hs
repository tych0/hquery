module Hquery.Transform where

import Text.XmlHtml
import Text.XmlHtml.Cursor

import Hquery.Selector

transform :: CssSel -> Cursor -> Node -> Node
transform sel cur insert = transformR sel cur insert
  where
    transformR sel cur insert =
      case sel of
        Id name attrSel =
