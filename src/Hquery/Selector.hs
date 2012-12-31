module Hquery.Selector where

import Data.Text
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

data AttrMod = Remove | Append deriving Show

data AttrSel = AttrSel Text (Maybe AttrMod) deriving Show

data CssSel =
  Id Text (Maybe AttrSel) |
  Name Text (Maybe AttrSel) |
  Class Text (Maybe AttrSel) |
  Attr Text Text (Maybe AttrSel) |  -- [first=second], special cases for name, id?
  Elem Text (Maybe AttrSel) |
  Star (Maybe AttrSel)
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

attrSelParser :: Parser (Maybe AttrSel)
attrSelParser = optionMaybe selParser
  where
    selParser :: Parser AttrSel
    selParser = do { m_reservedOp "["
                   ; name <- m_identifier
                   ; mod <- attrModParser
                   ; m_reservedOp "]"
                   ; return (AttrSel (pack name) mod)
                   }

cssSelParser :: Parser CssSel
cssSelParser = m_whiteSpace >> selParser
  where
    selParser :: Parser CssSel
    selParser = do { m_reservedOp "."
                   ; name <- m_identifier
                   ; attrSel <- attrSelParser
                   ; return (Class (pack name) attrSel)
                   }
                <|> do { m_reservedOp "#"
                       ; name <- m_identifier
                       ; attrSel <- attrSelParser
                       ; return (Id (pack name) attrSel)
                       }
                <|> do { m_reservedOp "["
                       ; attr <- m_identifier
                       ; m_reservedOp "="
                       ; value <- m_identifier
                       ; m_reservedOp "]"
                       ; attrSel <- attrSelParser
                       ; return (Attr (pack attr) (pack value) attrSel)
                       }
                <|> do { id <- m_identifier
                       ; attrSel <- attrSelParser
                       ; return (Elem (pack id) attrSel)
                       }
                <|> do { m_reservedOp "*"
                       ; attrSel <- attrSelParser
                       ; return (Star attrSel)
                       }

extractAttrSel :: CssSel -> Maybe AttrSel
extractAttrSel sel = case sel of
  Id _ s -> s
  Name _ s -> s
  Class _ s -> s
  Attr _ _ s -> s
  Elem _ s -> s
  Star s -> s
