module Main where

import Hquery

run :: String -> IO ()
run input
        = case hq input of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

main =
  do{ run ".foo";
    ; run "#bar";
    ; run "#bar [baz+]";
    ; run "* [bar]"
    ; run "[foo=baz] [bar]"
    }
