module Doxotry.Language.Typing where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.Writer (class MonadWriter)
import Data.List as List
import Data.Maybe (maybe)
import Data.Newtype (over, unwrap)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Doxotry.Language.Common (Log, tellLog)
import Doxotry.Language.Grammar (Tm, TmLit(..), Tm_(..), Ty(..), TyBase(..), TyCtx(..), Var, prettyTm, prettyTy, prettyTyCtx, prettyVar)
import Prim.Row (class Lacks)
import Record as Record
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

type Ctx = { tyCtx :: TyCtx }

mkCtx :: Ctx
mkCtx =
  { tyCtx: TyCtx none
  }

type Env = {}

mkEnv :: Env
mkEnv =
  {}

newtype Error = Error
  { message :: String
  }

derive newtype instance Show Error

derive newtype instance Eq Error

--------------------------------------------------------------------------------

type TypedTm an = Tm (TypedAn an)
type TypedAn an = (ty :: Ty | an)

--------------------------------------------------------------------------------

checkTm
  :: forall m an
   . MonadReader Ctx m
  => MonadThrow Error m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => Show (Record an)
  => Ty
  -> Tm an
  -> m (TypedTm an)
-- LitTm
checkTm ty@(BaseTy { base: StringTyBase }) tm0@(LitTm tl@({ lit: StringTmLit _ }) an) = do
  log_checkTm ty tm0
  pure $ LitTm tl (Record.insert (Proxy @"ty") ty an)
checkTm ty@(BaseTy { base: NumberTyBase }) tm0@(LitTm tl@({ lit: NumberTmLit _ }) an) = do
  log_checkTm ty tm0
  pure $ LitTm tl (Record.insert (Proxy @"ty") ty an)
-- Var
checkTm ty tm0@(VarTm tm an) = do
  log_checkTm ty tm0
  ty' <- getTypeOfVar tm.var
  unless (ty == ty') do
    throwError $ Error { message: "var " <> prettyTm tm0 <> " was expected to have type " <> prettyTy ty <> ", but it actually has type " <> prettyTy ty' }
  pure $ VarTm { var: tm.var } (Record.insert (Proxy @"ty") ty an)
-- AppTm
checkTm ty tm0@(AppTm tm an) = do
  log_checkTm ty tm0
  apl <- case tm.apl of
    FunTm apl _ -> pure apl
    apl -> throwError $ Error { message: "the applicant of an application must be a function term, but it actually was " <> prettyTm apl }
  arg' <- checkTm apl.dom tm.arg
  body' <-
    extendTyCtx apl.prm apl.dom do
      checkTm ty apl.body
  pure $ AppTm
    { apl:
        FunTm
          { prm: apl.prm, dom: apl.dom, body: body' }
          (Record.insert (Proxy @"ty") (FunTy { prm: apl.prm, dom: apl.dom, cod: ty }) an)
    , arg: arg'
    }
    (Record.insert (Proxy @"ty") ty an)

-- FunTm
checkTm ty tm0@(FunTm tm an) = do
  log_checkTm ty tm0
  funTy <- case ty of
    FunTy funTy -> pure funTy
    _ -> throwError $ Error { message: "The term " <> prettyTm tm0 <> " was expected to have a non-function type " <> prettyTy ty <> ", but it is actually a function term" }
  unless (funTy.dom == tm.dom) do
    throwError $ Error { message: "The term " <> prettyTm tm0 <> " was expected to be a function term with domain " <> prettyTy funTy.dom <> ", but it actually had domain " <> prettyTy tm.dom }
  body' <-
    extendTyCtx tm.prm tm.dom do
      checkTm funTy.cod tm.body
  pure $
    FunTm
      { prm: tm.prm, dom: tm.dom, body: body' }
      (Record.insert (Proxy @"ty") ty an)
-- InputTm
checkTm ty tm0@(InputTm tm an) = do
  log_checkTm ty tm0
  pure $
    InputTm
      { prompt: tm.prompt }
      (Record.insert (Proxy @"ty") ty an)
-- type error
checkTm ty tm = do
  tellLog "checkTm" $ prettyTm tm <> " : " <> prettyTy ty
  throwError $ Error { message: "The term " <> prettyTm tm <> " was expected to have type " <> prettyTy ty <> ", but it can't have that type." }

log_checkTm
  :: forall m an
   . MonadReader Ctx m
  => MonadWriter (Array Log) m
  => Ty
  -> Tm_ (Record an)
  -> m Unit
log_checkTm ty tm = do
  ctx <- ask
  tellLog "checkTm" $ prettyTyCtx ctx.tyCtx <> " |- " <> prettyTm tm <> " : " <> prettyTy ty

extendTyCtx :: forall m a. MonadReader Ctx m => Var -> Ty -> m a -> m a
extendTyCtx x ty ma = local (\ctx -> ctx { tyCtx = ctx.tyCtx # over TyCtx (List.Cons (x /\ ty)) }) ma

--------------------------------------------------------------------------------

getTypeOfVar :: forall m. MonadReader Ctx m => MonadThrow Error m => Var -> m Ty
getTypeOfVar x = do
  ctx <- ask
  ctx.tyCtx
    # unwrap
    # List.findMap
        ( \(x' /\ ty) -> do
            guard $ x == x'
            pure ty
        )
    # maybe (throwError $ Error { message: "Unrecognized variable find a variable " <> prettyVar x <> " in context " <> prettyTyCtx ctx.tyCtx }) pure
