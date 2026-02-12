module Doxotry.Language.Syntax where

import Control.Category (identity)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Maybe (Maybe(..))
import Doxotry.Language.Grammar as G

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

mkStringTy :: G.Ty
mkStringTy = G.BaseTy { base: G.StringTyBase }

stringTy :: G.Ty
stringTy = mkStringTy

mkNumberTy :: G.Ty
mkNumberTy = G.BaseTy { base: G.NumberTyBase }

numberTy :: G.Ty
numberTy = mkNumberTy

mkArrTy :: String -> G.Ty -> G.Ty -> G.Ty
mkArrTy x dom cod = G.FunTy { prm: var x, dom, cod }

arrTy1 :: String -> G.Ty -> (G.Ty -> G.Ty)
arrTy1 prm dom = \cod -> mkArrTy prm dom cod

arrTy3 :: Array (G.Ty -> G.Ty) -> G.Ty -> G.Ty
arrTy3 ks cod = foldr identity cod ks

infix 101 arrTy1 as &:

infixr 100 arrTy3 as &->

--------------------------------------------------------------------------------
-- Terms
--------------------------------------------------------------------------------

mkNumberTm :: Number -> G.Tm ()
mkNumberTm v = G.LitTm { lit: G.NumberTmLit v } {}

number :: Number -> G.Tm ()
number = mkNumberTm

mkStringTm :: String -> G.Tm ()
mkStringTm v = G.LitTm { lit: G.StringTmLit v } {}

string :: String -> G.Tm ()
string = mkStringTm

mkVarTm :: String -> G.Tm ()
mkVarTm name = G.VarTm { var: G.Var { name, mb_index: Nothing } } {}

ref :: String -> G.Tm ()
ref = mkVarTm

mkAppTm :: G.Tm () -> G.Tm () -> G.Tm ()
mkAppTm apl arg = G.AppTm { apl, arg } {}

app :: G.Tm () -> G.Tm () -> G.Tm ()
app = mkAppTm

apps :: forall f. Foldable f => G.Tm () -> f (G.Tm ()) -> G.Tm ()
apps f args = foldl app f args

infix 100 apps as &

mkLamTm :: String -> G.Ty -> G.Tm () -> G.Tm ()
mkLamTm prm dom body = G.LamTm { prm: var prm, dom, body } {}

fun :: String -> G.Ty -> G.Tm () -> G.Tm ()
fun = mkLamTm

mkInputTm :: String -> G.Tm ()
mkInputTm prompt = G.InputTm { prompt } {}

input :: String -> G.Tm ()
input = mkInputTm

mkVar :: String -> G.Var
mkVar name = G.Var { name, mb_index: Nothing }

var :: String -> G.Var
var = mkVar
