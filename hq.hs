module Main where

import Text.ParserCombinators.Parsec

simple :: Parser Char
simple  = letter

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

main =
  do{ run simple "a";
    ; run simple "bc";
    ; run simple "z"
    }
