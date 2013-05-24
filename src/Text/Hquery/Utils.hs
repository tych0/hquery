-- | This module exports various useful utility functions for working with
-- XmlHtml Nodes. For example, the equality operator on Node does a structural
-- comparison of the nodes. However, this is not entirely useful, since
-- transformed nodes may be equal but e.g. have their attributes in a different
-- order in the list. Among other things, this module defines an EqNode type
-- which has an Eq instance that does semantic equality instead of structural
-- equality.
{-# LANGUAGE OverloadedStrings #-}
module Text.Hquery.Utils (
  -- * Types
  EqNode(..),

  -- * Functions
  -- ** Equality
  attrsEq,
  nodeEq,

  -- ** Utility
  stripWhitespaceNodes,
  flattenTextNodes,
  mapChildren
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.XmlHtml
import Text.XmlHtml.Cursor
import Data.Maybe


newtype EqNode = EqNode Node deriving Show
instance Eq (EqNode) where
  (EqNode n1) == (EqNode n2) = nodeEq n1 n2

-- | Test a list of attributes for equality. This has special handling for the
-- "class" attribute, so that the order in which the classes are applied to the
-- node doesn't matter. Additionally, the oder of the attributes in either list
-- is also ignored.
attrsEq :: [(T.Text, T.Text)] -> [(T.Text, T.Text)] -> Bool
attrsEq attrs1 attrs2 = do
  let (m1, class1) = mapNoClass attrs1
  let (m2, class2) = mapNoClass attrs2
  m1 == m2 && classEq class1 class2
  where
    mapNoClass xs = do
      let m = Map.fromList xs
      (Map.delete "class" m, Map.lookup "class" m)
    classEq Nothing Nothing = True
    classEq (Just class1) (Just class2) = do
      let s1 = Set.fromList (T.words class1)
      let s2 = Set.fromList (T.words class2)
      s1 == s2
    classEq _ _ = False

-- | A top level semantic node equality funciton.
nodeEq :: Node -> Node -> Bool
nodeEq (TextNode t1) (TextNode t2) = t1 == t2
nodeEq (Comment t1) (Comment t2) = t1 == t2
nodeEq Element { elementTag = tag1
               , elementAttrs = attrs1
               , elementChildren = kids1
               }
       Element { elementTag = tag2
               , elementAttrs = attrs2
               , elementChildren = kids2
               }
       = tag1 == tag2
       && attrsEq attrs1 attrs2
       && length kids1 == length kids2
       && all (uncurry nodeEq) (zip kids1 kids2)
nodeEq _ _ = False

-- | Strip nodes that contain only whitespace. This can be useful when doing
-- equality comparisons of trees (e.g. in testing). XmlHtml keeps all
-- whitespace, which can cause structural equality differences in trees which
-- were produced programattically vs. hand written and nicely formatted trees.
stripWhitespaceNodes :: Node -> Maybe Node
stripWhitespaceNodes (TextNode t1) | T.null (T.strip (t1)) = Nothing
stripWhitespaceNodes e @ Element { elementChildren = kids } =
  Just (e { elementChildren = mapMaybe stripWhitespaceNodes kids })
stripWhitespaceNodes x = Just x

flattenTextNodes :: [Node] -> [Node]
flattenTextNodes = foldr appendElts []
  where
    appendElts :: Node -> [Node] -> [Node]
    appendElts (TextNode last) (TextNode prev : rest) =
      TextNode (last ++ prev) : rest
    appendElts n ns = n : ns
    flattenElement :: Node -> Node
    flattenElement e @ Element { elementChildren = kids } =
      e { elementChildren = flattenTextNodes kids }
    flattenElement = id


mapChildren :: ([Node] -> [Node]) -> Cursor -> Cursor
mapChildren f c =
  case current c of
    e @ Element { elementChildren = kids } -> setNode (e { elementChildren = f kids }) c
    _ -> c
