module Doxotry.Language.Grammar where

import Prelude

import Data.Tuple.Nested (type (/\))

--------------------------------------------------------------------------------

data Ty
  = NumberTy
  | StringTy
  | VarTy VarTy

type VarTy = { var :: TyVar }

--------------------------------------------------------------------------------

data Tm ty
  = LitTm (LitTm ty)
  | VarTm (VarTm ty)
  | AppTm (AppTm ty)
  | FunTm (FunTm ty)
  | CloTm (CloTm ty)

type LitTm ty = { lit :: Lit, ty :: ty }

type VarTm ty = { var :: TmVar, ty :: ty }

type AppTm ty = { apl :: Tm ty, arg :: Tm ty, ty :: ty }

type FunTm ty = { prm :: TmVar, body :: Tm ty, ty :: ty }

type CloTm ty = { env :: ExeEnv ty, body :: Tm ty, ty :: ty }

--------------------------------------------------------------------------------

newtype TyCtx = Ctx (Array (String /\ Ty))

newtype ExeEnv ty = Env (Array (String /\ Tm ty))

newtype TmVar = TmVar String

newtype TyVar = TyVar String

--------------------------------------------------------------------------------

data Lit
  = NumberLit Number
  | StringLit String

