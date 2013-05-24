{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Parsec
import Text.Parsec.Error

import Text.Hquery
import Text.Hquery.Utils
import Text.Hquery.Internal.Selector

tests :: [(String, (CssSel, Maybe AttrSel))]
tests = [ ("div", (Elem "div", Nothing))
        , (".elt", (Class "elt", Nothing))
        , (".elt *", (Class "elt", Just CData))
        , (".elt [class+]", (Class "elt", Just $ AttrSel "class" AppendAttr))
        , (".elt [class]", (Class "elt", Just $ AttrSel "class" Set))
        ]

makeTest :: (String, (CssSel, Maybe AttrSel)) -> Test
makeTest (sel, expected) = do
  let errorToString e = Left (unwords (map messageString (errorMessages e)))
      result = either errorToString Right (parse commandParser "" sel)
  testCase sel (assertEqual sel (Right expected) result)

main :: IO ()
main = defaultMain (map makeTest tests)
