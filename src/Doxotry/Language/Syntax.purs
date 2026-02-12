module Doxotry.Language.Syntax where

import Data.Foldable (class Foldable, foldl, foldr)
import Data.Maybe (Maybe(..))
import Doxotry.Language.Grammar as G

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

stringTy :: G.Ty
stringTy = G.BaseTy { base: G.StringTyBase }

numberTy :: G.Ty
numberTy = G.BaseTy { base: G.NumberTyBase }

arrTy :: String -> G.Ty -> G.Ty -> G.Ty
arrTy x dom cod = G.FunTy { prm: var x, dom, cod }

arrsTy :: forall f. Foldable f => f Bind -> G.Ty -> G.Ty
arrsTy xs cod = foldr (\(Bind x) -> arrTy x.prm x.dom) cod xs

infixr 100 arrsTy as &->

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

number :: Number -> G.Tm ()
number v = G.LitTm { lit: G.NumberTmLit v } {}

string :: String -> G.Tm ()
string v = G.LitTm { lit: G.StringTmLit v } {}

ref :: String -> G.Tm ()
ref name = G.VarTm { var: G.Var { name, mb_index: Nothing } } {}

app :: G.Tm () -> G.Tm () -> G.Tm ()
app apl arg = G.AppTm { apl, arg } {}

apps :: forall f. Foldable f => G.Tm () -> f (G.Tm ()) -> G.Tm ()
apps f args = foldl app f args

infixl 110 apps as &

lam :: String -> G.Ty -> G.Tm () -> G.Tm ()
lam prm dom body = G.LamTm { prm: var prm, dom, body } {}

lams :: forall f. Foldable f => f Bind -> G.Tm () -> G.Tm ()
lams prms body = foldr (\(Bind x) -> lam x.prm x.dom) body prms

infixr 100 lams as &=>

input :: String -> G.Tm ()
input prompt = G.InputTm { prompt } {}

var :: String -> G.Var
var name = G.Var { name, mb_index: Nothing }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

newtype Bind = Bind { prm :: String, dom :: G.Ty }

mkBind :: String -> G.Ty -> Bind
mkBind prm dom = Bind { prm: prm, dom }

infix 101 mkBind as &:

