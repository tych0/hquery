module Main where

import Hquery

run :: String -> IO ()
run input = do
  let _ = hq input ""
  print "hello"

main =
  do{ run ".foo"
    ; run "#bar"
    ; run "#bar [baz+]"
    ; run "* [bar]"
    ; run "[foo=baz] [bar]"
    }
