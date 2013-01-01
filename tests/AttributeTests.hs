module Main (main) where

import Control.Exception
import Data.Typeable
import qualified Data.ByteString.Char8 as BS
import Test.HUnit hiding (Node)
import Text.XmlHtml

import System.FilePath

import Hquery

data TestException = TestException String deriving (Show, Typeable)
instance Exception TestException

tests :: [(String, String -> Node -> Node, String)]
tests = [ ("#foo [class+]", \s -> hq s ("bar" :: String), "AddClass")
        ]

makeTests :: [(String, String -> Node -> Node, String)] -> IO Test
makeTests xs = do
  ts <- mapM makeTest xs
  print ts
  return (TestList ts)
  where
    readInputAndExpected :: String -> IO (String, String)
    readInputAndExpected name = do
      let path = ("tests/attributes/" ++ name ++ ".html")
      print ("reading stuff for " ++ path)
      inp <- readFile path
      exp <- readFile (path <.> "expected")
      return (inp, exp)
    makeTest (sel, builder, fname) = do
      (inp, exp) <- readInputAndExpected fname
      let parsedInp = toHTML (sel ++ " input") inp
      let parsedExp = toHTML (sel ++ " expected") exp
      let xform = builder sel
      let result = xform parsedInp
      print (parsedExp == result)
      return (sel ~: parsedExp ~=? result)
      where
        docToNode doc = case doc of
                          HtmlDocument { docContent = n : _ } -> n -- There is a trailing (TextNode "\n")
                          _ -> throw (TestException (sel ++ "'s inp/exp is not a single node" ++ show doc))
        toHTML name inp = do
          let result = parseHTML name (BS.pack inp)
          either (\s -> throw (TestException s)) docToNode result

main :: IO Counts
main = do
  print ("running!")
  toRun <- makeTests (tests)
  runTestTT toRun
