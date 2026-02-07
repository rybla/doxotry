module Doxotry.Language.Typing where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Data.List as List
import Data.Maybe (maybe)
import Data.Newtype (over, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Doxotry.Language.Grammar (Tm(..), TmLit(..), TmVar, Ty(..), TyBase(..), TyCtx(..))

--------------------------------------------------------------------------------

type Ctx = { tyCtx :: TyCtx }

type Env = {}

newtype TypeError = TypeError
  { message :: String
  }

--------------------------------------------------------------------------------

type CheckedTm an = Tm (Ty /\ an)

checkTm
  :: forall m an
   . MonadReader Ctx m
  => MonadThrow TypeError m
  => Show an
  => Ty
  -> Tm an
  -> m (CheckedTm an)
checkTm ty@(BaseTy { base: StringTyBase }) (LitTm tl@({ lit: StringTmLit _ }) an) = pure $ LitTm tl (ty /\ an)
checkTm ty@(BaseTy { base: NumberTyBase }) (LitTm tl@({ lit: NumberTmLit _ }) an) = pure $ LitTm tl (ty /\ an)
checkTm ty tm0@(VarTm tm an) = do
  ty' <- getTypeOfTmVar tm.var
  unless (ty == ty') do
    throwError $ TypeError { message: "var " <> show tm0 <> " was expected to have type " <> show ty <> ", but it actually has type " <> show ty' }
  pure $ VarTm { var: tm.var } (ty /\ an)
checkTm ty (AppTm tm an) = do
  apl <- case tm.apl of
    FunTm apl _ -> pure apl
    apl -> throwError $ TypeError { message: "the applicant of an application must be a function term, but it actually was " <> show apl }
  arg' <- checkTm apl.dom tm.arg
  body' <- checkTm ty apl.body
  pure $ AppTm
    { apl:
        FunTm
          { prm: apl.prm, dom: apl.dom, body: body' }
          (FunTy { prm: apl.prm, dom: apl.dom, cod: ty } /\ an)
    , arg: arg'
    }
    (ty /\ an)
checkTm ty tm0@(FunTm tm an) = do
  funTy <- case ty of
    FunTy funTy -> pure funTy
    _ -> throwError $ TypeError { message: "The term " <> show tm0 <> " was expected to have a non-function type " <> show ty <> ", but it is actually a function term" }
  unless (funTy.dom == tm.dom) do
    throwError $ TypeError { message: "The term " <> show tm <> " was expected to be a function term with domain " <> show funTy.dom <> ", but it actually had domain " <> show tm.dom }
  body' <-
    extendTyCtx tm.prm tm.dom do
      checkTm funTy.cod tm.body
  pure $
    FunTm
      { prm: tm.prm, dom: tm.dom, body: body' }
      (ty /\ an)
checkTm ty tm = throwError $ TypeError { message: "The term " <> show tm <> " was expected to have type " <> show ty <> ", but it can't have that type." }

extendTyCtx :: forall m a. MonadReader Ctx m => TmVar -> Ty -> m a -> m a
extendTyCtx x ty ma = local (\ctx -> ctx { tyCtx = ctx.tyCtx # over TyCtx (List.Cons (x /\ ty)) }) ma

--------------------------------------------------------------------------------

getTypeOfTmVar :: forall m. MonadReader Ctx m => MonadThrow TypeError m => TmVar -> m Ty
getTypeOfTmVar x = do
  ctx <- ask
  ctx.tyCtx
    # unwrap
    # List.findMap
        ( \(x' /\ ty) -> do
            guard $ x == x'
            pure ty
        )
    # maybe (throwError $ TypeError { message: "Could not find a variable in context of the name " <> show x }) pure

