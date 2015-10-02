module Data.Expr.Proposition.Substitute where

import Data.List      (union)

import Data.Expr.Proposition.Types

-- ----------------------------------------
-- variable substitution

type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env (Lit a) = Lit a
substVars env (Var a) = case lookup a env of
							Nothing -> Var a
							Just b -> b
substVars env (Unary op e) = Unary op (substVars env e)
substVars env (Binary op e1 e2) = Binary op (substVars env e1) (substVars env e2)


freeVars :: Expr -> [Ident]
freeVars (Lit a) = []
freeVars (Var a) = [a]
freeVars (Unary _ e) = freeVars e
freeVars (Binary _ e1 e2) = freeVars e1 `union` freeVars e2

-- ----------------------------------------
