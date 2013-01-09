module Main (main) where

import Control.Exception
import Data.Typeable
import qualified Data.ByteString.Char8 as BS
import Test.HUnit hiding (Node, Test)
import Text.XmlHtml

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit

import System.FilePath

import Hquery
import Hquery.Utils

data TestException = TestException String deriving (Show, Typeable)
instance Exception TestException

tests :: [(String, String -> Node -> Node, String)]
tests = [ ("#foo [class+]", \s -> hq s "bar", "AddClass")
        , ("div [class+]", \s -> hq s "bar", "AddClass")
        , ("div [class!]", \s -> hq s "baz", "RemoveClass")
        , (".bar [class!]", \s -> hq s "baz", "RemoveClass")
        , ("#foo [id!]", \s -> hq s "baz", "RemoveId")
        , (".elt *", \s -> hq s ["one", "two", "three"], "PopulateList")
        , ("thisshouldnotmatch", \s -> hq s "", "Noop")
        , ("div [class+]", \s -> hq s "bar", "AppendClass")
        ]

makeTests :: [(String, String -> Node -> Node, String)] -> IO [Test]
makeTests xs = mapM makeTest xs
  where
    readInputAndExpected :: String -> IO (String, String)
    readInputAndExpected name = do
      let path = ("tests/attributes/" ++ name ++ ".html")
      inp <- readFile path
      exp <- readFile (path <.> "expected")
      return (inp, exp)
    makeTest (sel, builder, fname) = do
      (inp, exp) <- readInputAndExpected fname
      let parsedInp = toHTML (sel ++ " input") inp
      let parsedExp = toHTML (sel ++ " expected") exp
      let xform = builder sel
      let result = fmap EqNode (stripWhitespaceNodes (xform parsedInp))
      let expected = fmap EqNode (stripWhitespaceNodes (parsedExp))
      return (testCase sel (assertEqual sel expected result))
      where
        docToNode doc = case doc of
                          HtmlDocument { docContent = n : _ } -> n -- There is a trailing (TextNode "\n")
                          _ -> throw (TestException (sel ++ "'s inp/exp is not a single node" ++ show doc))
        toHTML name inp = do
          let result = parseHTML name (BS.pack inp)
          either (\s -> throw (TestException s)) docToNode result

main :: IO ()
main = do
  toRun <- makeTests (tests)
  defaultMain toRun

