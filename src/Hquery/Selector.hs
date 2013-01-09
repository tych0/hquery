module Hquery.Selector where

import Data.Text
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

data AttrMod = Remove | Append | Set deriving (Show, Eq)

data AttrSel = AttrSel Text AttrMod deriving (Show, Eq)

data CssSel =
  Id Text |
  Name Text |
  Class Text |
  Attr Text Text |  -- [first=second], special cases for name, id?
  Elem Text |
  Star
  deriving (Show, Eq)

def = emptyDef{ identStart = letter
              , identLetter = alphaNum
              }

TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def

attrModParser :: Parser AttrMod
attrModParser = option Set modParser
  where
    modParser :: Parser AttrMod
    modParser =   (m_reservedOp "+" >> return Append)
              <|> (m_reservedOp "!" >> return Remove)

attrSelParser :: Parser (Maybe AttrSel)
attrSelParser = optionMaybe selParser
  where
    selParser :: Parser AttrSel
    selParser = do { m_reservedOp "["
                   ; name <- m_identifier
                   ; m <- attrModParser
                   ; m_reservedOp "]"
                   ; return (AttrSel (pack name) m)
                   }

cssSelParser :: Parser CssSel
cssSelParser = do { m_reservedOp "."
                  ; name <- m_identifier
                  ; return (Class (pack name))
                  }
           <|> do { m_reservedOp "#"
                  ; name <- m_identifier
                  ; return (Id (pack name))
                  }
           <|> do { m_reservedOp "["
                  ; attr <- m_identifier
                  ; m_reservedOp "="
                  ; value <- m_identifier
                  ; m_reservedOp "]"
                  ; return (Attr (pack attr) (pack value))
                  }
           <|> do { id_ <- m_identifier
                  ; return (Elem (pack id_))
                  }
           <|> do { m_reservedOp "*"
                  ; return Star
                  }

commandParser :: Parser (CssSel, Maybe AttrSel)
commandParser = m_whiteSpace >> parse
  where
    parse :: Parser (CssSel, Maybe AttrSel)
    parse = do
      css <- cssSelParser
      attr <- attrSelParser
      _ <- eof
      return (css, attr)
