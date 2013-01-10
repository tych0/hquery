{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Node, Test)

import Hquery
import Hquery.Utils

import Text.XmlHtml

elt :: T.Text -> [(T.Text, T.Text)] -> [Node] -> Node
elt tag attrs kids = Element { elementTag = tag
                             , elementAttrs = attrs
                             , elementChildren = kids
                             }

mkDiv :: [Node] -> Node
mkDiv = elt "div" []

nodeEqTests :: [(Node, Node, Bool)]
nodeEqTests = [ (TextNode "foo", TextNode "foo", True)
              , (Comment "foo", Comment "foo", True)
              , (TextNode "foo", TextNode "bar", False)
              , (Comment "foo", Comment "bar", False)
              , (TextNode "foo", Comment "foo", False)
              , (mkDiv [TextNode "foo"], mkDiv [TextNode "foo"], True)
              , (mkDiv [TextNode "bar"], mkDiv [TextNode "foo"], False)
              , (mkDiv [mkDiv []], mkDiv [mkDiv []], True)
              , (mkDiv [mkDiv [TextNode "foo"]], mkDiv [mkDiv [TextNode "bar"]], False)
              , (mkDiv [mkDiv [TextNode "foo"]], mkDiv [], False)
              ]

main :: IO ()
main =
  let makeEqNodeTest (n1, n2, t) =
        let eq1 = EqNode n1
            eq2 = EqNode n2
            str = (show n1) ++ " == " ++ (show n2)
        in testCase "nodeEqTestcase" (assertEqual str t (eq1 == eq2))
      nodeTests = map makeEqNodeTest nodeEqTests
  in defaultMain nodeTests
