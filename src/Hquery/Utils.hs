module Hquery.Utils where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Text.XmlHtml

newtype EqNode a = EqNode a deriving Show
instance Eq (EqNode Node) where
  (EqNode n1) == (EqNode n2) = nodeEq n1 n2

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
       =  tag1 == tag2
       && attrsEq attrs1 attrs2
       && all (uncurry nodeEq) (zip kids1 kids2)
nodeEq _ _ = False

