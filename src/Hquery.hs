{-# LANGUAGE DeriveDataTypeable #-}
module Hquery where

import Control.Monad
import Control.Exception
import Data.Typeable
import Data.List

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
  hq sel target = parseSel sel buildNodeXform
    where
      buildNodeXform (css, attr) = case attr of
        Just _ -> id -- TODO: error handling? can't insert node in attr?
        Nothing -> transform css (setNode target)

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
      replaceCurrent ns c = let curN = current c in
        case fmap (\c -> (current c, c)) (parent c) of
          Just (pn @ Element {elementChildren = kids }, pc) -> do
            -- the current node should be a child of the parent
            let idx = maybe (raise "idx bug!") id (findIndex ((==) curN) kids)
            let next = setNode (pn { elementChildren = concat (map replaceN kids) }) pc
            -- the next has the right number of children
            maybe (raise "bug!") id (getChild (idx - 1 + (length ns)) next)
            where
              replaceN n2 = if n2 == curN then ns else [n2]
          -- FIXME: shouldn't be a non-Element node, because of parent call
          Just _ -> raise "bug! no non-Element nodes as parents!"
          -- XXX: BUG: if you replace the root node with an empty list of
          -- nodes, nothing happens.
          Nothing -> maybe c id (fromNodes ns)
