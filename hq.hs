module Main where

-- import Text.ParserCombinators.Parsec
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Parsec.Combinator

import Data.Maybe

data AttrMod = Remove | Append deriving Show

data AttrSelT = AttrSel String (Maybe AttrMod) deriving Show

data CssSel =
  Id String (Maybe AttrSelT) |
  Name String (Maybe AttrSelT) |
  Class String (Maybe AttrSelT)
  --Attr String String           |  -- [first=second], special cases for name, id?
  --Elem String (Maybe AttrSelT) |
  --Star (Maybe AttrSelT)
  deriving Show
  -- Colon?

def = emptyDef{identStart = letter
              , identLetter = alphaNum
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

{-
cssSelParser :: Parser CssSel
cssSelParser = buildExpressionParser table term <?> "expression"
table = [ [Prefix (m_reservedOp "." >> Class)]
        , [Prefix (m_reservedOp "#" >> Id)]
        ]
-}

attrModParser :: Parser (Maybe AttrMod)
attrModParser = optionMaybe modParser
  where
    modParser :: Parser AttrMod
    modParser =   (m_reservedOp "+" >> return Append)
              <|> (m_reservedOp "!" >> return Remove)

attrSelParser :: Parser (Maybe AttrSelT)
attrSelParser = optionMaybe selParser
  where
    selParser :: Parser AttrSelT
    selParser = do { m_reservedOp "["
                   ; name <- m_identifier
                   ; mod <- attrModParser
                   ; m_reservedOp "]"
                   ; return (AttrSel name mod)
                   }

cssSelParser :: Parser CssSel
cssSelParser = m_whiteSpace >> selParser
  where
    selParser :: Parser CssSel
    selParser = do { m_reservedOp "."
                   ; name <- m_identifier
                   ; attrSel <- attrSelParser
                   ; return (Class name attrSel)
                   }
                <|> do { m_reservedOp "#"
                       ; name <- m_identifier
                       ; attrSel <- attrSelParser
                       ; return (Id name attrSel)
                       }

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x

main =
  do{ run cssSelParser ".foo";
    ; run cssSelParser "#bar";
    ; run cssSelParser "#bar [baz+]";
    }
