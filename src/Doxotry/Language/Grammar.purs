module Doxotry.Language.Grammar where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe)
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

type Tm an = Tm_ (Record an)

data Tm_ :: Type -> Type
data Tm_ an
  = LitTm LitTm an
  | VarTm VarTm an
  | AppTm (AppTm_ an) an
  | FunTm (FunTm_ an) an
  | CloTm (CloTm_ an) an
  | InputTm InputTm an

derive instance Generic (Tm_ an) _

instance Show an => Show (Tm_ an) where
  show x = genericShow x

derive instance Eq an => Eq (Tm_ an)

derive instance Functor Tm_

type LitTm = { lit :: TmLit }

type VarTm = { var :: TmVar }

type AppTm an = AppTm_ (Record an)
type AppTm_ an = { apl :: Tm_ an, arg :: Tm_ an }

type FunTm an = FunTm_ (Record an)
type FunTm_ an = { prm :: TmVar, dom :: Ty, body :: Tm_ an }

type CloTm an = CloTm_ (Record an)
type CloTm_ an = { env :: ExeEnv_ an, body :: Tm_ an }

type InputTm = { prompt :: String }

getAnOfTm :: forall an. Tm_ an -> an
getAnOfTm (LitTm _ an) = an
getAnOfTm (VarTm _ an) = an
getAnOfTm (AppTm _ an) = an
getAnOfTm (FunTm _ an) = an
getAnOfTm (CloTm _ an) = an
getAnOfTm (InputTm _ an) = an

modifySurfaceAnOfTm :: forall an. (an -> an) -> Tm_ an -> Tm_ an
modifySurfaceAnOfTm f (LitTm tm an) = LitTm tm (f an)
modifySurfaceAnOfTm f (VarTm tm an) = VarTm tm (f an)
modifySurfaceAnOfTm f (AppTm tm an) = AppTm tm (f an)
modifySurfaceAnOfTm f (FunTm tm an) = FunTm tm (f an)
modifySurfaceAnOfTm f (CloTm tm an) = CloTm tm (f an)
modifySurfaceAnOfTm f (InputTm tm an) = InputTm tm (f an)

--------------------------------------------------------------------------------

type SemTm m an = SemTm_ m (Record an)

data SemTm_ m an
  = SynSemTm (Tm_ an)
  | FunSemTm (FunSemTm m an) an

type FunSemTm m an = { name :: String, run :: SemTm_ m an -> m (SemTm_ m an) }

getAnOfSemTm :: forall m an. SemTm_ m an -> an
getAnOfSemTm (SynSemTm tm) = getAnOfTm tm
getAnOfSemTm (FunSemTm _ an) = an

--------------------------------------------------------------------------------

newtype TyCtx = TyCtx (List (TmVar /\ Ty))

derive instance Newtype TyCtx _

derive newtype instance Show TyCtx

derive newtype instance Eq TyCtx

--------------------------------------------------------------------------------

type ExeEnv an = ExeEnv_ (Record an)

newtype ExeEnv_ :: Type -> Type
newtype ExeEnv_ an = ExeEnv (List (TmVar /\ Tm_ an))

derive instance Newtype (ExeEnv_ an) _

derive newtype instance Show an => Show (ExeEnv_ an)

derive newtype instance Eq an => Eq (ExeEnv_ an)

derive instance Functor ExeEnv_

--------------------------------------------------------------------------------

newtype TmVar = TmVar { name :: String, mb_index :: Maybe Int }

derive instance Newtype TmVar _

derive newtype instance Show TmVar

derive newtype instance Eq TmVar

--------------------------------------------------------------------------------

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

