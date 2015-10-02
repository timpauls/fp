-- | the visitor pattern for Expr

module Data.Expr.Proposition.Visitors where

import Data.Expr.Proposition.Types
import Data.Expr.Proposition.Visit
import Data.Expr.Proposition.Eval (mf1, mf2)

import Data.Set(Set)
import qualified Data.Set as S

-- ----------------------------------------

type Idents = Set Ident

freeVars :: Expr -> Idents
freeVars
  = visit V {
  				vLit = \x -> S.empty,
  				vVar = \x -> S.singleton x,
  				vUnary = \op x -> x,
  				vBinary = \op s1 s2 -> S.union s1 s2
			}
    
type VarEnv = [(Ident, Expr)]

substVars :: VarEnv -> Expr -> Expr
substVars env
  = visit idExpr {
  				vVar = \ident -> case lookup ident env of
  							Nothing -> Var ident
  							Just a -> a
  				}

eval :: Expr -> Bool
eval
  = visit V {
  				vLit = id,
  				vVar = \x -> error $ unwords ["free variable", show x, "in expression"],
  				vUnary = \op x -> mf1 op x,
  				vBinary = \op x1 x2 -> mf2 op x1 x2
			}

-- ----------------------------------------
