module Main (main) where

import Test.HUnit
import Text.XmlHtml

import Hquery

main :: IO Counts
main = do
  let template = Element { elementTag = "div"
                         , elementAttrs = [("id", "foo")]
                         , elementChildren = []
                         }
  let sel = hq "#foo [class+]" ("bar" :: String)
  let expected = Element { elementTag = "div"
                         , elementAttrs = [ ("id", "foo")
                                          , ("class", "bar")
                                          ]
                         , elementChildren = []
                         }
  let tests = "test1" ~: "#foo [class+]" ~: expected ~=? (sel template)
  runTestTT tests
