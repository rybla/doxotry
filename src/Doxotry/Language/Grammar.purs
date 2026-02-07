module Doxotry.Language.Grammar where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))

--------------------------------------------------------------------------------

data Ty
  = BaseTy BaseTy
  | FunTy FunTy

type BaseTy = { base :: TyBase }

type FunTy = { prm :: TmVar, dom :: Ty, cod :: Ty }

derive instance Generic Ty _

instance Show Ty where
  show x = genericShow x

instance Eq Ty where
  eq x = genericEq x

data TyBase
  = NumberTyBase
  | StringTyBase

derive instance Generic TyBase _

instance Show TyBase where
  show x = genericShow x

derive instance Eq TyBase

--------------------------------------------------------------------------------

data Tm an
  = LitTm LitTm an
  | VarTm VarTm an
  | AppTm (AppTm an) an
  | FunTm (FunTm an) an
  | CloTm (CloTm an) an

derive instance Generic (Tm an) _

instance Show an => Show (Tm an) where
  show x = genericShow x

derive instance Eq an => Eq (Tm an)

type LitTm = { lit :: TmLit }

type VarTm = { var :: TmVar }

type AppTm an = { apl :: Tm an, arg :: Tm an }

type FunTm an = { prm :: TmVar, dom :: Ty, body :: Tm an }

type CloTm an = { env :: ExeEnv an, body :: Tm an }

--------------------------------------------------------------------------------

newtype TyCtx = TyCtx (List (TmVar /\ Ty))

derive instance Newtype TyCtx _

derive newtype instance Show TyCtx

derive newtype instance Eq TyCtx

newtype ExeEnv an = Env (List (String /\ Tm an))

derive newtype instance Show an => Show (ExeEnv an)

derive newtype instance Eq an => Eq (ExeEnv an)

newtype TmVar = TmVar String

derive newtype instance Show TmVar

derive newtype instance Eq TmVar

newtype TyVar = TyVar String

derive newtype instance Show TyVar

derive newtype instance Eq TyVar

--------------------------------------------------------------------------------

data TmLit
  = NumberTmLit Number
  | StringTmLit String

derive instance Eq TmLit

derive instance Generic TmLit _

instance Show TmLit where
  show x = genericShow x

