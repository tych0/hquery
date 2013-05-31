-- | ADTs for representing what node operations to perform, and their parsers.
module Text.Hquery.Internal.Selector (
  -- * Types
  AttrMod(..),
  AttrSel(..),
  CssSel(..),
  Matchable(..),

  -- * Parsers
  attrModParser,
  attrSelParser,
  cssSelParser,
  commandParser
  ) where

import Data.Text
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

import Control.Applicative

data AttrMod = Remove | AppendAttr | Set deriving (Show, Eq)

data AttrSel =
  AttrSel Text AttrMod |
  CData |
  Append
  deriving (Show, Eq)

data CssSel =
  Id Text |
  Name Text |
  Class Text |
  Attr Text Text |  -- [first=second], special cases for name, id?
  Elem Text |
  Star
  deriving (Show, Eq)

m_identifier :: Parser String
rop :: String -> Parser ()
TokenParser{ identifier = m_identifier
           , reservedOp = rop
           } = makeTokenParser emptyDef{ identStart = letter
                                       , identLetter = alphaNum
                                       }

idp :: Parser Text
idp = pack <$> m_identifier

attrModParser :: Parser AttrMod
attrModParser = option Set $
      (AppendAttr <$ rop "+")
  <|> (Remove <$ rop "!")

attrSelParser :: Parser (Maybe AttrSel)
attrSelParser = optionMaybe selParser
  where
    selParser :: Parser AttrSel
    selParser =
          AttrSel <$> (rop "[" *> idp) <*> attrModParser <* rop "]"
      <|> CData <$ rop "*"
      <|> Append <$ rop "+"

cssSelParser :: Parser CssSel
cssSelParser = Class <$> (rop "." *> idp)
           <|> Id <$> (rop "#" *> idp)
           <|> Attr <$> (rop "[" *> idp) <*> (rop "=" *> idp <* rop "]")
           <|> Star <$ rop "*"
           <|> Elem <$> idp

commandParser :: Parser (CssSel, Maybe AttrSel)
commandParser = (,) <$> (cssSelParser <* spaces) <*> attrSelParser

data Matchable =
  Sel CssSel |
  RSel CssSel Matchable
  deriving (Show, Eq)
