module Hquery where

import Text.Parsec
import Hquery.Selector

data DomTransformer =
  StringXform CssSel (Maybe AttrSel) String |
  ListStringXform CssSel (Maybe AttrSel) [String] |
  InvalidXform String
  deriving Show

parseSel :: String -> ((CssSel, Maybe AttrSel) -> DomTransformer) -> DomTransformer
parseSel sel f =
  either
    (\ err -> InvalidXform (show err))
    f
    (parse commandParser "" sel)

class MakeTransformer a where
  hq :: String -> a -> DomTransformer

instance MakeTransformer String where
  hq sel target = parseSel sel (\(s, a) -> StringXform s a target)

instance MakeTransformer [String] where
  hq sel xs = parseSel sel (\(s, a) -> ListStringXform s a xs)
