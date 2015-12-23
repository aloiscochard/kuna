module Kuna.Java.Syn.PP where

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen

import qualified Data.Text as T

import Kuna.Internal.PP
import Kuna.Java.Syn

prettyJExpr :: JExpr -> Doc
prettyJExpr (JVar vname) = prettyVName vname
prettyJExpr (JConst cval) = red . text . show $ cval
prettyJExpr (JCall n ts es rt) = nest 2 $ prettyCName n <> (tupled $ fmap prettyJExpr es)
prettyJExpr (JLocal (BindLocal i exp tpe) b) = local i <> space <> keyword "=" <> prettyJExpr exp <$$> prettyJExpr b

prettyJType :: JType -> Doc
prettyJType = dullmagenta . text . show

prettyCName :: CName -> Doc
prettyCName cn  = dullcyan $ text $ show cn

prettyVName :: VName -> Doc
prettyVName (JLocalVar w _) = local w
