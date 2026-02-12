module Doxotry.Language.Syntax where

import Control.Category (identity)
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

arrTy1 :: String -> G.Ty -> (G.Ty -> G.Ty)
arrTy1 prm dom = \cod -> arrTy prm dom cod

arrTy3 :: Array (G.Ty -> G.Ty) -> G.Ty -> G.Ty
arrTy3 ks cod = foldr identity cod ks

infix 101 arrTy1 as &:

infixr 100 arrTy3 as &->

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

infixr 101 lam as &::

lams :: forall f. Foldable f => f (G.Tm () -> G.Tm ()) -> G.Tm () -> G.Tm ()
lams prms body = foldr identity body prms

infixr 100 lams as &=>

input :: String -> G.Tm ()
input prompt = G.InputTm { prompt } {}

var :: String -> G.Var
var name = G.Var { name, mb_index: Nothing }
