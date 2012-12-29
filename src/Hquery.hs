module Hquery where

import Text.Parsec
import Hquery.Selector

data DomTransformer =
  StringXform CssSel String |
  ListStringXform CssSel [String] |
  InvalidXform String
  deriving Show

parseSel :: String -> (CssSel -> DomTransformer) -> DomTransformer
parseSel sel f =
  either
    (\ err -> InvalidXform (show err))
    f
    (parse cssSelParser "" sel)

class MakeTransformer a where
  hq :: String -> a -> DomTransformer

instance MakeTransformer String where
  hq sel target = parseSel sel (\ s -> StringXform s target)
