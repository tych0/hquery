module Main where

import Hquery

run :: String -> IO ()
run input = print (hq input "")

main =
  do{ run ".foo"
    ; run "#bar"
    ; run "#bar [baz+]"
    ; run "* [bar]"
    ; run "[foo=baz] [bar]"
    }
