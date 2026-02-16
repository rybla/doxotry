module Doxotry.Language.Grammar where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable, foldl, foldr, intercalate)
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)

--------------------------------------------------------------------------------

data Ty
  = BaseTy BaseTy
  | ArrTy ArrTy

type BaseTy = { base :: TyBase }

type ArrTy = { prm :: Var, dom :: Ty, cod :: Ty }

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

prettyTy :: Ty -> String
prettyTy (BaseTy bt) = showTyBase bt.base
prettyTy (ArrTy ty) = "(" <> prettyVar ty.prm <> " : " <> prettyTy ty.dom <> " -> " <> prettyTy ty.cod <> ")"

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
  | LamTm (LamTm_ an) an
  | AppTm (AppTm_ an) an
  | GenerateTm (GenerateTm_ an) an

derive instance Generic (Tm_ an) _

instance Show an => Show (Tm_ an) where
  show x = genericShow x

derive instance Eq an => Eq (Tm_ an)

derive instance Functor Tm_

type LitTm = { lit :: TmLit }

type VarTm = { var :: Var }

type LamTm an = LamTm_ (Record an)
type LamTm_ an = { prm :: Var, dom :: Ty, body :: Tm_ an }

type AppTm an = AppTm_ (Record an)
type AppTm_ an = { apl :: Tm_ an, arg :: Tm_ an }

type GenerateTm an = GenerateTm_ (Record an)

type GenerateTm_ :: Type -> Type
type GenerateTm_ an = {}

data TmLit
  = NumberTmLit Number
  | StringTmLit String

derive instance Eq TmLit

derive instance Generic TmLit _

instance Show TmLit where
  show x = genericShow x

prettyTm :: forall an. Tm_ an -> String
prettyTm (LitTm tm _) = prettyLit tm.lit
prettyTm (VarTm tm _) = prettyVar tm.var
prettyTm (AppTm tm _) = "(" <> prettyTm tm.apl <> " " <> prettyTm tm.arg <> ")"
prettyTm (LamTm tm _) = "(" <> prettyVar tm.prm <> " :: " <> prettyTy tm.dom <> " => " <> prettyTm tm.body <> ")"
prettyTm (GenerateTm _tm _) = "#generate"

prettyLit :: TmLit -> String
prettyLit (NumberTmLit v) = show v
prettyLit (StringTmLit v) = show v

getAnOfTm :: forall an. Tm_ an -> an
getAnOfTm (LitTm _ an) = an
getAnOfTm (VarTm _ an) = an
getAnOfTm (AppTm _ an) = an
getAnOfTm (LamTm _ an) = an
getAnOfTm (GenerateTm _ an) = an

getDomOfTm :: forall an. Tm_ an -> Maybe { prm :: Var, dom :: Ty }
getDomOfTm (LamTm tm _) = pure { prm: tm.prm, dom: tm.dom }
getDomOfTm (GenerateTm _ _) = pure { prm: var "prompt", dom: stringTy }
getDomOfTm _ = none

modifySurfaceAnOfTm :: forall an. (an -> an) -> Tm_ an -> Tm_ an
modifySurfaceAnOfTm f (LitTm tm an) = LitTm tm (f an)
modifySurfaceAnOfTm f (VarTm tm an) = VarTm tm (f an)
modifySurfaceAnOfTm f (AppTm tm an) = AppTm tm (f an)
modifySurfaceAnOfTm f (LamTm tm an) = LamTm tm (f an)
modifySurfaceAnOfTm f (GenerateTm tm an) = GenerateTm tm (f an)

--------------------------------------------------------------------------------

type SemTm m an = SemTm_ m (Record an)

data SemTm_ m an
  = SynSemTm (Tm_ an)
  | LamSemTm (LamSemTm_ m an) an

type LamSemTm m an = LamSemTm_ m (Record an)
type LamSemTm_ m an = { prm :: Var, run :: SemTm_ m an -> m (SemTm_ m an) }

type GenerateSemTm m an = GenerateSemTm_ m (Record an)
type GenerateSemTm_ m an = { run :: m (SemTm_ m an) }

prettySemTm :: forall m an. SemTm m an -> String
prettySemTm (SynSemTm tm) = prettyTm tm
prettySemTm (LamSemTm tm _an) = "(" <> prettyVar tm.prm <> " => " <> "..." <> ")"

getAnOfSemTm :: forall m an. SemTm_ m an -> an
getAnOfSemTm (SynSemTm tm) = getAnOfTm tm
getAnOfSemTm (LamSemTm _ an) = an

--------------------------------------------------------------------------------

newtype TyCtx = TyCtx (List (Var /\ Ty))

derive instance Newtype TyCtx _

derive newtype instance Show TyCtx

derive newtype instance Eq TyCtx

prettyTyCtx :: TyCtx -> String
prettyTyCtx (TyCtx xs) = "{" <> (xs # map (\(x /\ ty) -> prettyVar x <> " : " <> prettyTy ty) # intercalate ", ") <> "}"

--------------------------------------------------------------------------------

newtype Var = Var { name :: String, mb_index :: Maybe Int }

derive instance Newtype Var _

derive newtype instance Show Var

derive newtype instance Eq Var

--------------------------------------------------------------------------------

newtype TyVar = TyVar String

derive newtype instance Show TyVar

derive newtype instance Eq TyVar

--------------------------------------------------------------------------------
-- Syntax for Types 
--------------------------------------------------------------------------------

stringTy :: Ty
stringTy = BaseTy { base: StringTyBase }

numberTy :: Ty
numberTy = BaseTy { base: NumberTyBase }

arrTy :: String -> Ty -> Ty -> Ty
arrTy x dom cod = ArrTy { prm: var x, dom, cod }

arrsTy :: forall f. Foldable f => f Bind -> Ty -> Ty
arrsTy xs cod = foldr (\(Bind x) -> arrTy x.prm x.dom) cod xs

infixr 100 arrsTy as &->

--------------------------------------------------------------------------------
-- Syntax for Terms
--------------------------------------------------------------------------------

number :: Number -> Tm ()
number v = LitTm { lit: NumberTmLit v } {}

string :: String -> Tm ()
string v = LitTm { lit: StringTmLit v } {}

ref :: String -> Tm ()
ref name = VarTm { var: Var { name, mb_index: none } } {}

app :: Tm () -> Tm () -> Tm ()
app apl arg = AppTm { apl, arg } {}

apps :: forall f. Foldable f => Tm () -> f (Tm ()) -> Tm ()
apps f args = foldl app f args

infixl 110 apps as &

lam :: String -> Ty -> Tm () -> Tm ()
lam prm dom body = LamTm { prm: var prm, dom, body } {}

lams :: forall f. Foldable f => f Bind -> Tm () -> Tm ()
lams prms body = foldr (\(Bind x) -> lam x.prm x.dom) body prms

infixr 100 lams as &=>

generate :: Tm ()
generate = GenerateTm {} {}

var :: String -> Var
var name = Var { name, mb_index: none }

--------------------------------------------------------------------------------
--  Syntax Utilities
--------------------------------------------------------------------------------

newtype Bind = Bind { prm :: String, dom :: Ty }

mkBind :: String -> Ty -> Bind
mkBind prm dom = Bind { prm: prm, dom }

infix 101 mkBind as &:

