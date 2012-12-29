module Hquery.Selector where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

data AttrMod = Remove | Append deriving Show

data AttrSelT = AttrSel String (Maybe AttrMod) deriving Show

data CssSel =
  Id String (Maybe AttrSelT) |
  Name String (Maybe AttrSelT) |
  Class String (Maybe AttrSelT) |
  Attr String String (Maybe AttrSelT) |  -- [first=second], special cases for name, id?
  Elem String (Maybe AttrSelT) |
  Star (Maybe AttrSelT)
  deriving Show

def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

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
                <|> do { m_reservedOp "["
                       ; attr <- m_identifier
                       ; m_reservedOp "="
                       ; value <- m_identifier
                       ; m_reservedOp "]"
                       ; attrSel <- attrSelParser
                       ; return (Attr attr value attrSel)
                       }
                <|> do { id <- m_identifier
                       ; attrSel <- attrSelParser
                       ; return (Elem id attrSel)
                       }
                <|> do { m_reservedOp "*"
                       ; attrSel <- attrSelParser
                       ; return (Star attrSel)
                       }

