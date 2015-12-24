module Kuna.Kore.Syn.PP where

import Prelude hiding ((<$>))
import Data.Char (chr)
import Bound (instantiate, instantiate1)
import Bound.Scope (bindings)
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Text as T
import qualified Data.Vector as V

import Kuna.Internal.PP
import Kuna.Kore.Syn hiding (bindings)

prettyExpr :: KoreExpr -> Doc
prettyExpr = prettyExpr' 0

prettyExpr' :: Int -> KoreExpr -> Doc
prettyExpr' _ (Var b) = prettyName b
prettyExpr' _ (Lit l) = red $ prettyLiteral l
prettyExpr' n (App lhs rhs) = prettyExpr'' n lhs <> space <> prettyExpr'' n rhs
prettyExpr' n (Fld p ok ko) =
  nest 2 $ keyword "if" <> prettyExpr' n p </>
    keyword "then" <> prettyExpr' n ok </>
    keyword "else" <> prettyExpr' n ko
prettyExpr' n (Lam scope) =
  keyword' "λ" <> local' name <> text "." </>
  (prettyExpr' (n + 1) $ instantiate1 x scope) where
    x = Var $ Name (T.pack $ "#" ++ name) Internal
    name = [chr $ n + 97]
prettyExpr' n (Let scopes expr) =
  (nest 2 $ keyword "let" <$> binds) <$> (nest 2 $ keyword "in" <$> body)
    where
      localVar i = Var $ Name (T.pack $ concat ["#", show i]) Internal
      binds = vsep . V.toList $ V.imap f bounds where
        f i e = local (show i) <> space <> keyword "=" <> prettyExpr' i e
      bounds = foldr f V.empty scopes where
        f scope xs = V.snoc xs $ instantiate localVar scope
      body = prettyExpr' n  $ instantiate localVar expr

prettyExpr'' :: Int -> KoreExpr -> Doc
prettyExpr'' n expr@(Var _)  = prettyExpr' n expr
prettyExpr'' n expr@(Lit _)  = prettyExpr' n expr
prettyExpr'' n expr          = lparen <> prettyExpr' n expr <> rparen

prettyName :: Name -> Doc
prettyName (Name id sort) = color $ text $ T.unpack id where
  color = case sort of
    Internal  -> dullgreen
    Machine   -> dullcyan

prettyLiteral :: Literal -> Doc
prettyLiteral (LitInt32 w) = text $ show w
