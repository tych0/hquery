module Hquery where

import qualified Data.Text as T
import Text.Parsec
import Text.XmlHtml
import Text.XmlHtml.Cursor
import Hquery.Selector
import Hquery.Transform

parseSel :: String -> ((CssSel, Maybe AttrSel) -> Node -> Node) -> Node -> Node
parseSel sel f =
  either
    (\_ -> id) -- TODO: error handling? invalid sel
    f
    (parse commandParser "" sel)

class MakeTransformer a where
  hq :: String -> a -> Node -> Node

instance MakeTransformer String where
  hq sel target = parseSel sel buildStringXform
    where
      buildStringXform (css, attr) = do
        let dflt = (setNode (TextNode (T.pack target)))
        let hasMod = (\s -> buildAttrMod s (T.pack target))
        let nodeXform = maybe dflt hasMod attr
        transform css nodeXform

instance MakeTransformer [String] where
  hq sel xs = hq sel (map (\s -> TextNode (T.pack s)) xs)

instance MakeTransformer Node where
  hq sel target = parseSel sel buildNodeXform
    where
      buildNodeXform (css, attr) = case attr of
        Just _ -> id -- TODO: error handling? can't insert node in attr?
        Nothing -> transform css (setNode target)

instance MakeTransformer [Node] where
  hq sel ns = parseSel sel buildNodesXform
    where
      buildNodesXform (css, attr) = case attr of
        Just _ -> id -- TODO: error handling? can't insert nodes in an attr
        Nothing -> do
          let insertNodes = insertManyRight ns
          let result cur = removeGoRight (insertNodes cur)
          let replaced cur = maybe cur id (result cur)
          transform css replaced
