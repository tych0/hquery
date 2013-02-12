module Main (main) where

import Control.Exception
import Data.Typeable
import Data.Maybe
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Test.HUnit hiding (Node, Test)
import Text.XmlHtml

import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.HUnit

import System.FilePath

import Text.Hquery
import Text.Hquery.Utils

data TestException = TestException String deriving (Show, Typeable)
instance Exception TestException

peopleTest :: [Node] -> [Node]
peopleTest =
  let people = [ ("Bob", "Engineer")
               , ("Sally", "CEO")
               , ("Rutherford", "Hacker")
               , ("Vikki", "Hybrid")
               ]
      bind (name, occupation) =
        hq ".name *" name . hq ".occupation *" occupation

  in hq ".person" (map bind people)

groupTest :: [Node]
groupTest = map (mkSpan . T.pack . show) [1..3]
  where
    mkSpan s = Element (T.pack "span") [] $ [TextNode s]

tests :: [([Node] -> [Node], String)]
tests = [ (hq "#foo [class+]" "bar", "AddClass")
        , (hq "div [class+]" "bar", "AddClass")
        , (hq "div [class!]" "baz", "RemoveClass")
        , (hq ".bar [class!]" "baz", "RemoveClass")
        , (hq "#foo [id!]" "baz", "RemoveId")
        , (hq ".foo *" "bar", "PopulateString")
        , (hq ".elt *" ["one", "two", "three"], "PopulateList")
        , (hq "thisshouldnotmatch" "", "Noop")
        , (hq "div [class+]" "bar", "AppendClass")
        , ( (hq ".name *" "Aaron Swartz") . (hq ".address *" "aaronsw@example.com")
          , "BasicComposition"
          )
        , (peopleTest, "PeopleOccupations")
        , (hq ".foo *" "bar", "NestXform")
        , (hq ".foo" "bar", "NestReplace")
        , (hq "#foo" nothing, "RemoveNode")
        , (hq ".foo *" $ Group groupTest, "GroupNodes")
        , (hq "*" nothing, "RemoveStar")
        ]

makeTests :: [([Node] -> [Node], String)] -> IO [Test]
makeTests xs = mapM makeTest xs
  where
    readInputAndExpected :: String -> IO (String, String)
    readInputAndExpected name = do
      let path = ("tests/markup/" ++ name ++ ".html")
      inp <- readFile path
      exp <- readFile (path <.> "expected")
      return (inp, exp)
    makeTest (f, testName) = do
      (inp, exp) <- readInputAndExpected testName
      let parsedInp = toHTML (testName ++ " input") inp
      let parsedExp = toHTML (testName ++ " expected") exp
      let result = comparable (f parsedInp)
      let expected = comparable parsedExp
      return (testCase testName (assertEqual testName expected result))
      where
        comparable ns = map EqNode $ catMaybes (map stripWhitespaceNodes ns)
        docToNode doc = case doc of
                          HtmlDocument { docContent = content } -> content
                          _ -> throw (TestException (testName ++ "'s inp/exp is not a single node" ++ show doc))
        toHTML name inp = do
          let result = parseHTML name (BS.pack inp)
          either (throw . TestException) docToNode result

main :: IO ()
main = do
  toRun <- makeTests (tests)
  defaultMain toRun
