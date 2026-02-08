module Doxotry.Language.Grammar where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\))

--------------------------------------------------------------------------------

data Ty
  = BaseTy BaseTy
  | FunTy FunTy

type BaseTy = { base :: TyBase }

type FunTy = { prm :: Var, dom :: Ty, cod :: Ty }

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

mkStringTy :: Ty
mkStringTy = BaseTy { base: StringTyBase }

mkNumberTy :: Ty
mkNumberTy = BaseTy { base: NumberTyBase }

mkFunTy :: Var -> Ty -> Ty -> Ty
mkFunTy prm dom cod = FunTy { prm, dom, cod }

prettyTy :: Ty -> String
prettyTy (BaseTy bt) = showTyBase bt.base
prettyTy (FunTy ty) = "(" <> prettyVar ty.prm <> " : " <> prettyTy ty.dom <> ") -> " <> prettyTy ty.cod

showTyBase :: TyBase -> String
showTyBase NumberTyBase = "Number"
showTyBase StringTyBase = "String"

prettyVar :: Var -> String
prettyVar (Var x) = x.name <> maybe "" (\i -> "@" <> show i) x.mb_index

--------------------------------------------------------------------------------

type Tm an = Tm_ (Record an)

data Tm_ :: Type -> Type
data Tm_ an
  = LitTm LitTm an
  | VarTm VarTm an
  | AppTm (AppTm_ an) an
  | FunTm (FunTm_ an) an
  | InputTm InputTm an

derive instance Generic (Tm_ an) _

instance Show an => Show (Tm_ an) where
  show x = genericShow x

derive instance Eq an => Eq (Tm_ an)

derive instance Functor Tm_

type LitTm = { lit :: TmLit }

type VarTm = { var :: Var }

type AppTm an = AppTm_ (Record an)
type AppTm_ an = { apl :: Tm_ an, arg :: Tm_ an }

type FunTm an = FunTm_ (Record an)
type FunTm_ an = { prm :: Var, dom :: Ty, body :: Tm_ an }

type InputTm = { prompt :: String }

data TmLit
  = NumberTmLit Number
  | StringTmLit String

derive instance Eq TmLit

derive instance Generic TmLit _

instance Show TmLit where
  show x = genericShow x

prettyTm :: forall an. Tm an -> String
prettyTm (LitTm tm _) = prettyLit tm.lit
prettyTm (VarTm tm _) = prettyVar tm.var
prettyTm (AppTm tm _) = "(" <> prettyTm tm.apl <> " " <> prettyTm tm.arg <> ")"
prettyTm (FunTm tm _) = "(" <> prettyVar tm.prm <> " : " <> prettyTy tm.dom <> " -> " <> prettyTm tm.body <> ")"
prettyTm (InputTm tm _) = "(#input " <> show tm.prompt <> ")"

prettyLit :: TmLit -> String
prettyLit (NumberTmLit v) = show v
prettyLit (StringTmLit v) = show v

mkNumberLitTm :: Number -> Tm ()
mkNumberLitTm v = LitTm { lit: NumberTmLit v } {}

mkStringLitTm :: String -> Tm ()
mkStringLitTm v = LitTm { lit: StringTmLit v } {}

mkVar :: Var -> Tm ()
mkVar var = VarTm { var } {}

mkAppTm :: Tm () -> Tm () -> Tm ()
mkAppTm apl arg = AppTm { apl, arg } {}

mkFunTm :: Var -> Ty -> Tm () -> Tm ()
mkFunTm prm dom body = FunTm { prm, dom, body } {}

mkInputTm :: String -> Tm ()
mkInputTm prompt = InputTm { prompt } {}

getAnOfTm :: forall an. Tm_ an -> an
getAnOfTm (LitTm _ an) = an
getAnOfTm (VarTm _ an) = an
getAnOfTm (AppTm _ an) = an
getAnOfTm (FunTm _ an) = an
getAnOfTm (InputTm _ an) = an

modifySurfaceAnOfTm :: forall an. (an -> an) -> Tm_ an -> Tm_ an
modifySurfaceAnOfTm f (LitTm tm an) = LitTm tm (f an)
modifySurfaceAnOfTm f (VarTm tm an) = VarTm tm (f an)
modifySurfaceAnOfTm f (AppTm tm an) = AppTm tm (f an)
modifySurfaceAnOfTm f (FunTm tm an) = FunTm tm (f an)
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

newtype TyCtx = TyCtx (List (Var /\ Ty))

derive instance Newtype TyCtx _

derive newtype instance Show TyCtx

derive newtype instance Eq TyCtx

--------------------------------------------------------------------------------

type ExeEnv an = ExeEnv_ (Record an)

newtype ExeEnv_ :: Type -> Type
newtype ExeEnv_ an = ExeEnv (List (Var /\ Tm_ an))

derive instance Newtype (ExeEnv_ an) _

derive newtype instance Show an => Show (ExeEnv_ an)

derive newtype instance Eq an => Eq (ExeEnv_ an)

derive instance Functor ExeEnv_

--------------------------------------------------------------------------------

newtype Var = Var { name :: String, mb_index :: Maybe Int }

derive instance Newtype Var _

derive newtype instance Show Var

derive newtype instance Eq Var

--------------------------------------------------------------------------------

newtype TyVar = TyVar String

derive newtype instance Show TyVar

derive newtype instance Eq TyVar

