{-# LANGUAGE OverloadedStrings #-}
{- | This module exports the top level constructors
used for building node transformations.
For example, if your template is

> <div class="person">
>   <div class="name"></div>
>   <div class="occupation"></div>
> </div>

and you invoke hquery like this:

> import Text.Hquery
> template = ... -- parse your template here
> people = [ ("Justin Bieber", "Celebrity")
>          , ("Jens Kidman", "Musician")
>          ]
> bindPerson (n, o) = hq ".name *" n . hq ".occupation *" o
> f = hq ".person *" $ map bindPerson people
> f template

you'll get markup like this:

> <div class="person">
>   <div class="name">Justin Bieber</div>
>   <div class="occupation">Celebrity</div>
> </div>
> <div class="person">
>   <div class="name">Jens Kidman</div>
>   <div class="occupation">Musician</div>
> </div>

You can also add, remove, and append to element attributes. For example if we
have: @ \<div class=\"foo\"\>\</div\> @, below are some example
transformations:

  * @ hq \"div [class+]\" \"hidden\" @ gives @ \<div class=\"foo hidden\"\>\</div\> @

  * @ hq \".foo [id]\" \"bar\" @ gives @ \<div id=\"bar\" class=\"foo\"\>\</div\> @

  * @ hq \"* [class!]\" \"foo\" @ gives @ \<div\>\</div\> @

This module exports several constructors for common types of node
transformations. These constructors simply give you back a @ 'Node' -> 'Node'
@, which you can then apply however you choose.
-}

module Text.Hquery (
  -- * Constructors
  MakeTransformer(..),
  Group(..),

  -- * Values
  -- | nothing is handy for deleting a node from the tree, you cna replace it
  -- with nothing, e.g. @ hq \".foo\" nothing @
  nothing,
  ) where

import Data.List
import Data.Maybe

import qualified Data.Text as T
import Text.Parsec
import Text.XmlHtml
import Text.XmlHtml.Cursor
import Text.Hquery.Internal.Error
import Text.Hquery.Internal.Selector
import Text.Hquery.Internal.Transform
import Text.Hquery.Utils

parseSel :: String ->
            (Maybe AttrSel -> Cursor -> Maybe Cursor) ->
            [Node] ->
            [Node]
parseSel sel builder = case parse commandParser "" sel of
  Left _ -> id -- TODO: error handling? invalid sel
  Right (css, attr) -> transform css (builder attr)

nothing :: [Node]
nothing = []

data Group = Group [Node]

class MakeTransformer a where
  hq :: String -> a -> [Node] -> [Node]

instance MakeTransformer a => MakeTransformer (Maybe a) where
  hq sel Nothing = hq sel nothing
  hq sel (Just t) = hq sel t

instance MakeTransformer String where
  hq sel target = parseSel sel nodeXform
    where
      packed = T.pack target
      n = TextNode packed
      nodeXform attr = Just . case attr of
        Just (AttrSel t m) -> buildAttrMod t m packed
        Just Append -> mapChildren (++ [n])
        Just CData -> mapChildren (const [n])
        Nothing -> setNode n

instance MakeTransformer [String] where
  hq sel = hq sel . map (TextNode . T.pack)

instance MakeTransformer Node where
  hq sel target = hq sel [target]

instance MakeTransformer Group where
  hq sel (Group ns) = parseSel sel groupXform
    where
      groupXform attr c = case (attr, current c) of
        (Just CData, e @ Element {}) -> Just $ setNode (e { elementChildren = ns }) c
        (Just _, _) -> Just c -- TODO: error handling?
        (Nothing, _) -> replaceCurrent ns c

instance MakeTransformer ([Node] -> [Node]) where
  hq sel f = hq sel [f]

instance MakeTransformer [[Node] -> [Node]] where
  hq sel fs = parseSel sel (\_ -> replicateAndApply)
    where
      replicateAndApply c = let n = (current c)
                                ns = concat $ fmap ($[n]) fs
                            in replaceCurrent ns c

instance MakeTransformer [Node] where
  hq sel ns = parseSel sel buildNodesXform
    where
      buildNodesXform (Just CData) = replicateNode
      buildNodesXform (Just Append) = Just . mapChildren (++ ns)
      buildNodesXform (Just (AttrSel _ _)) = Just -- TODO: error handling? can't insert nodes in an attr
      buildNodesXform Nothing = replaceCurrent ns
      replicateNode :: Cursor -> Maybe Cursor
      replicateNode c = let n = (current c) in
        case n of
          e @ Element {} ->
            let replicated = map (\x -> e { elementChildren = [x] }) ns
            in replaceCurrent replicated c
          _ -> raise "bug: shouldn't be replicating on a non-Element node"

replaceCurrent :: [Node] -> Cursor -> Maybe Cursor
replaceCurrent ns c = fromMaybe dflt $ do
  p <- parent c
  case current p of
    pn@Element { elementChildren = kids } -> do
      ix <- elemIndex curN kids
      let next = setNode (pn { elementChildren = concatMap replaceN kids }) p
      let childIdx = (ix - 1 + (length ns))
      return $ Just $ fromMaybe next $ getChild childIdx next
    _ -> raise "should be no non-Element parents!"
  where
    curN = current c
    replaceN n2 = if n2 == curN then ns else [n2]
    dflt = do
      newCur <- (fromNodes ns)
      return (fromMaybe newCur $ findRight isLast newCur)
