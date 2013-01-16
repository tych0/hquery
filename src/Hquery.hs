module Hquery where

import Data.List
import Data.Maybe

import qualified Data.Text as T
import Text.Parsec
import Text.XmlHtml
import Text.XmlHtml.Cursor
import Hquery.Error
import Hquery.Selector
import Hquery.Transform

parseSel :: String ->
            (Maybe AttrSel -> Cursor -> Cursor) ->
            [Node] ->
            [Node]
parseSel sel builder = case parse commandParser "" sel of
  Left _ -> id -- TODO: error handling? invalid sel
  Right (css, attr) -> transform css (builder attr)

class MakeTransformer a where
  hq :: String -> a -> [Node] -> [Node]

instance MakeTransformer String where
  hq sel target = parseSel sel nodeXform
    where
      nodeXform attr c = case (attr, current c) of
        (Just CData, e @ Element {}) -> setNode (e { elementChildren = [TextNode (T.pack target)] }) c
        -- the non-Element case isn't relevant here, since we can't match non-Elements
        (Just s, _) -> buildAttrMod s (T.pack target) c
        (Nothing, _) -> (setNode (TextNode (T.pack target))) c

instance MakeTransformer [String] where
  hq sel xs = hq sel (map (TextNode . T.pack) xs)

instance MakeTransformer Node where
  hq sel target = hq sel [target]

instance MakeTransformer ([Node] -> [Node]) where
  hq sel f = hq sel [f]

instance MakeTransformer [[Node] -> [Node]] where
  hq sel fs = parseSel sel (\_ -> replicateAndApply)
    where
      replicateAndApply c = let n = (current c)
                                ns = concat $ fmap ($[n]) fs
                            in replaceCurrent ns c

instance MakeTransformer [Node] where
  hq sel ns = parseSel sel buildNodesXform
    where
      buildNodesXform (Just CData) = replicateNode
      buildNodesXform (Just _) = id -- TODO: error handling? can't insert nodes in an attr
      buildNodesXform Nothing = replaceCurrent(ns)
      replicateNode :: Cursor -> Cursor
      replicateNode c = let n = (current c) in
        case n of
          e @ Element {} ->
            let replicated = map (\x -> e { elementChildren = [x] }) ns
            in replaceCurrent replicated c
          _ -> raise "bug: shouldn't be replicating on a non-Element node"

replaceCurrent :: [Node] -> Cursor -> Cursor
replaceCurrent ns c = fromMaybe dflt $ do
  p <- parent c
  case current p of
    pn@Element { elementChildren = kids } -> do
      ix <- elemIndex curN kids
      let next = setNode (pn { elementChildren = concatMap replaceN kids }) p
      getChild (ix - 1 + (length ns)) next
    _ -> raise "should be no non-Element parents!"
  where
    curN = current c
    replaceN n2 = if n2 == curN then ns else [n2]
    dflt = fromMaybe c $ do
      newCur <- (fromNodes ns)
      endCur <- findRight isLast newCur
      return endCur
