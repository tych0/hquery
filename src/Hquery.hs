module Hquery where

import Text.Parsec
import Hquery.Selector

hq :: String -> Either ParseError CssSel
hq = parse cssSelParser ""
