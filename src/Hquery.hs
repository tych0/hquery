{-# LANGUAGE DeriveDataTypeable #-}
module Hquery where

import Control.Monad
import Control.Exception
import Data.Typeable
import Data.List
import Data.Maybe

import qualified Data.Text as T
import Text.Parsec
import Text.XmlHtml
import Text.XmlHtml.Cursor
import Hquery.Selector
import Hquery.Transform

data HqueryInternalException = HqueryInternalException String
  deriving (Show, Typeable)
instance Exception HqueryInternalException

raise :: String -> a
raise s = throw (HqueryInternalException s)

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
        let dflt c = case (attr, current c) of
                       (Just CData, e @ Element {}) -> setNode (e { elementChildren = [TextNode (T.pack target)] }) c
                       _ -> (setNode (TextNode (T.pack target))) c
        let hasMod = (\s -> buildAttrMod s (T.pack target))
        let nodeXform = maybe dflt hasMod attr
        transform css nodeXform

instance MakeTransformer [String] where
  hq sel xs = hq sel (map (\s -> TextNode (T.pack s)) xs)

instance MakeTransformer Node where
  hq sel target = hq sel [target]

instance MakeTransformer [Node] where
  hq sel ns = parseSel sel buildNodesXform
    where
      buildNodesXform (css, attr) = case attr of
        Just CData -> transform css replicateNode
        Just _ -> id -- TODO: error handling? can't insert nodes in an attr
        Nothing -> transform css (replaceCurrent ns)
      replicateNode :: Cursor -> Cursor
      replicateNode c = let n = (current c) in
        case n of
          e @ Element {} ->
            let replicated = map (\x -> e { elementChildren = [x] }) ns
            in replaceCurrent replicated c
          _ -> c -- FIXME: bug: shouldn't be replicating on a non-Element node
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
          dflt = fromMaybe c (fromNodes ns)
