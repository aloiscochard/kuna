module Kuna.Kore.Syn.PP where

import Prelude hiding ((<$>))
import Bound (instantiate)
import Bound.Scope (bindings)
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Text as T
import qualified Data.Vector as V

import Kuna.Kore.Syn hiding (bindings)

prettyExpr :: KoreExpr -> Doc
prettyExpr (Var b) = prettyName b
prettyExpr (Lit l) = red $ prettyLiteral l
prettyExpr (App lhs rhs) = prettyExpr' lhs <> space <> prettyExpr' rhs
prettyExpr (Fld p ok ko) =
  nest 2 $ keyword "if" <> prettyExpr p <$>
    keyword "then" <> prettyExpr ok <$>
    keyword "else" <> prettyExpr ko
prettyExpr (Let scopes expr) =
  (nest 2 $ keyword "let" <$> binds) <$> (nest 2 $ keyword "in" <$> body)
    where
      local i = Var $ Name (T.pack $ concat ["#", show i]) Internal
      binds = vsep . V.toList $ V.imap f bounds where
        f i e = dullgreen (text $ concat ["#", show i]) <> space <> keyword "=" <> prettyExpr e
      bounds = foldr f V.empty scopes where
        f scope xs = V.snoc xs $ instantiate local scope
      body = prettyExpr $ instantiate local expr

prettyExpr' :: KoreExpr -> Doc
prettyExpr' expr@(Var _)  = prettyExpr expr
prettyExpr' expr@(Lit _)  = prettyExpr expr
prettyExpr' expr          = lparen <> prettyExpr expr <> rparen

prettyName :: Name -> Doc
prettyName (Name id sort) = color $ text $ T.unpack id where
  color = case sort of
    Internal  -> dullgreen
    Machine   -> dullcyan

prettyLiteral :: Literal -> Doc
prettyLiteral (LitInt32 w) = text $show w

keyword :: String -> Doc
keyword txt = dullyellow $ text txt <> space
