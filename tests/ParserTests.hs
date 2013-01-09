{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Text.Parsec

import Hquery
import Hquery.Selector
import Hquery.Utils

tests :: [(String, (CssSel, Maybe AttrSel))]
tests = [ ("div", (Elem "div", Nothing))
        , (".elt", (Class "elt", Nothing))
        , (".elt *", (Class "elt", Nothing))
        ]

makeTest :: (String, (CssSel, Maybe AttrSel)) -> Test
makeTest (sel, expected) = do
  let result = either (\_ -> Nothing) Just (parse commandParser "" sel)
  testCase sel (assertEqual sel (Just expected) result)

main :: IO ()
main = defaultMain (map makeTest tests)
