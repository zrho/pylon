{-# LANGUAGE TupleSections #-}
--------------------------------------------------------------------------------
module Language.Pylon.Core.Parser where
--------------------------------------------------------------------------------

import           Prelude hiding (pi)
import           Language.Pylon.Core.AST
import           Text.Trifecta
import qualified Text.Parser.Token           as T
import qualified Text.Parser.Token.Highlight as TH
import           Control.Applicative hiding (Const)
import           Control.Monad (unless, void)
import qualified Data.HashSet as HS
import qualified Data.Map     as M

--------------------------------------------------------------------------------
-- Identifiers
--------------------------------------------------------------------------------

reserved :: HS.HashSet String
reserved = HS.fromList
  [ "case"
  , "of"
  , "data"
  , "let"
  , "in"
  , "Type"
  ]

varStyle :: T.IdentifierStyle Parser
varStyle = T.IdentifierStyle
  { T._styleName              = "variable identifier"
  , T._styleStart             = lower    <|> oneOf "_"
  , T._styleLetter            = alphaNum <|> oneOf "_"
  , T._styleReserved          = reserved
  , T._styleHighlight         = TH.Identifier
  , T._styleReservedHighlight = TH.ReservedIdentifier
  }

conStyle :: T.IdentifierStyle Parser
conStyle = T.IdentifierStyle
  { T._styleName              = "constructor"
  , T._styleStart             = upper
  , T._styleLetter            = alphaNum <|> oneOf "_"
  , T._styleReserved          = reserved
  , T._styleHighlight         = TH.Constructor
  , T._styleReservedHighlight = TH.ReservedConstructor
  }

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

expP :: Parser Exp
expP = piP

appP :: Parser Exp
appP = do
  x  <- simpleP
  xs <- many simpleP
  return $ foldl EApp x xs

piP :: Parser Exp
piP = go <|> appP where
  go = do
    (i, t) <- T.parens typedIdent
    e      <- (T.symbol "->" *> piP)
    return $ EPi i t e

typedIdent :: Parser (Ident, Type)
typedIdent = (,)
  <$> T.ident varStyle
  <*> (T.symbol ":" *> simpleP)

--------------------------------------------------------------------------------
-- Let Expressions
--------------------------------------------------------------------------------

letP :: Parser Exp
letP = (\(i, t, b) e -> ELet i t b e)
  <$> (T.symbol "let" *> T.braces letBindingP)
  <*> (T.symbol "in"  *> expP)
  <?> "let expression"

letBindingP :: Parser (Ident, Type, Exp)
letBindingP = (,,)
  <$> (T.ident varStyle)
  <*> (T.symbol ":" *> simpleP)
  <*> (T.symbol "=" *> expP)
  <?> "let binding"

--------------------------------------------------------------------------------
-- Simple Expressions
--------------------------------------------------------------------------------

simpleP :: Parser Exp
simpleP =
      T.parens expP
  <|> lamP
  <|> letP
  <|> constP
  <|> varP
  <?> "simple expression"

varP :: Parser Exp
varP = EVar <$> T.ident varStyle    <?> "global identifier"

lamP :: Parser Exp
lamP = do
  (i, t) <- T.symbol "\\" *> typedIdent
  e      <- T.symbol "->" *> expP
  return $ ELam i t e
  <?> "lambda abstraction"

constP :: Parser Exp
constP = fmap EConst $
      ((CLit . LInt) <$> try T.integer       <?> "integer literal")
  <|> (const CUniv   <$> T.symbol "Type"     <?> "universe")
  <|> (CCon          <$> T.ident conStyle    <?> "constructor")
  <?> "constant expression"

--------------------------------------------------------------------------------
-- Top Level
--------------------------------------------------------------------------------

programP :: Parser Program
programP = do
  T.whiteSpace
  cons  <- fmap M.fromList $ many conDeclP
  binds <- fmap M.fromList $ many bindDeclP
  eof
  return $ Program cons binds

conDeclP :: Parser (Name, Con)
conDeclP = do
  T.symbol "data"
  name <- T.ident conStyle   <?> "constructor name"
  idx  <- T.parens T.natural <?> "constructor index"
  T.symbol "="
  ty   <- expP
  T.semi
  return (name, Con (fromIntegral idx) ty)
  <?> "constructor"

bindDeclP :: Parser (Name, Bind)
bindDeclP = do
  T.symbol "def"
  name <- T.ident varStyle <?> "binding name"
  ty   <- T.parens expP    <?> "binding type"
  T.symbol "="
  e    <- fmap Just $ T.braces $ semiSep matchP
  T.semi
  return (name, Bind e ty)
  <?> "binding"

matchP :: Parser Match
matchP = do
  vs  <- T.parens $ T.commaSep typedIdent
  lhs <- appP
  T.symbol "="
  rhs <- expP
  return $ Match vs lhs rhs
  <?> "match"

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

semiSepEndBy :: (Monad m, TokenParsing m) => m a -> m b -> m ([a], b)
semiSepEndBy p q = mid <|> end where
  mid = do
    a <- try p
    T.semi
    (as, e) <- semiSepEndBy p q
    return (a : as, e)
  end = fmap ([],) q
