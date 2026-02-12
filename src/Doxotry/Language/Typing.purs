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

type Err an = Err_ (Record an)
newtype Err_ an = Err
  { message :: String
  , subject :: Tm_ an
  }

derive newtype instance Show an => Show (Err_ an)

derive newtype instance Eq an => Eq (Err_ an)

--------------------------------------------------------------------------------

type TypedTm an = Tm (TypedAn an)
type TypedAn an = (ty :: Ty | an)

--------------------------------------------------------------------------------

typecheckTm
  :: forall m an
   . MonadReader Ctx m
  => MonadThrow (Err an) m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => Show (Record an)
  => Ty
  -> Tm an
  -> m (TypedTm an)
-- LitTm
typecheckTm ty@(BaseTy { base: StringTyBase }) tm0@(LitTm tl@({ lit: StringTmLit _ }) an) = do
  log_typecheckTm ty tm0
  pure $ LitTm tl (Record.insert (Proxy @"ty") ty an)
typecheckTm ty@(BaseTy { base: NumberTyBase }) tm0@(LitTm tl@({ lit: NumberTmLit _ }) an) = do
  log_typecheckTm ty tm0
  pure $ LitTm tl (Record.insert (Proxy @"ty") ty an)
-- Var
typecheckTm ty tm0@(VarTm tm an) = do
  log_typecheckTm ty tm0
  ty' <- getTypeOfVar tm.var an
  unless (ty == ty') do
    throwError $ Err
      { message: "var " <> prettyTm tm0 <> " was expected to have type " <> prettyTy ty <> ", but it actually has type " <> prettyTy ty'
      , subject: tm0
      }
  pure $ VarTm { var: tm.var } (Record.insert (Proxy @"ty") ty an)
-- AppTm
typecheckTm ty tm0@(AppTm tm an) = do
  log_typecheckTm ty tm0
  f <- case tm.apl of
    LamTm f _ -> pure f
    f -> throwError $ Err
      { message: "the applicant of an application must be a function term, but it actually was " <> prettyTm f
      , subject: tm0
      }
  arg' <- typecheckTm f.dom tm.arg
  b <-
    extendTyCtx f.prm f.dom do
      typecheckTm ty f.body
  pure $ AppTm
    { apl:
        LamTm
          { prm: f.prm, dom: f.dom, body: b }
          (Record.insert (Proxy @"ty") (FunTy { prm: f.prm, dom: f.dom, cod: ty }) an)
    , arg: arg'
    }
    (Record.insert (Proxy @"ty") ty an)

-- LamTm
typecheckTm ty tm0@(LamTm tm an) = do
  log_typecheckTm ty tm0
  phi <- case ty of
    FunTy phi -> pure phi
    _ -> throwError $ Err
      { message: "The term " <> prettyTm tm0 <> " was expected to have a non-function type " <> prettyTy ty <> ", but it is actually a function term"
      , subject: tm0
      }
  unless (phi.dom == tm.dom) do
    throwError $ Err
      { message: "The term " <> prettyTm tm0 <> " was expected to be a function term with domain " <> prettyTy phi.dom <> ", but it actually had domain " <> prettyTy tm.dom
      , subject: tm0
      }
  b <-
    extendTyCtx tm.prm tm.dom do
      typecheckTm phi.cod tm.body
  pure $
    LamTm
      { prm: tm.prm, dom: tm.dom, body: b }
      (Record.insert (Proxy @"ty") ty an)
-- InputTm
typecheckTm ty tm0@(InputTm tm an) = do
  log_typecheckTm ty tm0
  pure $
    InputTm
      { prompt: tm.prompt }
      (Record.insert (Proxy @"ty") ty an)
-- type error
typecheckTm ty tm = do
  tellLog "typecheckTm" $ prettyTm tm <> " : " <> prettyTy ty
  throwError $ Err
    { message: "The term " <> prettyTm tm <> " was expected to have type " <> prettyTy ty <> ", but it can't have that type."
    , subject: tm
    }

log_typecheckTm
  :: forall m an
   . MonadReader Ctx m
  => MonadWriter (Array Log) m
  => Ty
  -> Tm_ (Record an)
  -> m Unit
log_typecheckTm ty tm = do
  ctx <- ask
  tellLog "typecheckTm" $ prettyTyCtx ctx.tyCtx <> " |- " <> prettyTm tm <> " : " <> prettyTy ty

extendTyCtx :: forall m a. MonadReader Ctx m => Var -> Ty -> m a -> m a
extendTyCtx x ty ma = local (\ctx -> ctx { tyCtx = ctx.tyCtx # over TyCtx (List.Cons (x /\ ty)) }) ma

--------------------------------------------------------------------------------

getTypeOfVar :: forall m an. MonadReader Ctx m => MonadThrow (Err an) m => Var -> Record an -> m Ty
getTypeOfVar x an = do
  ctx <- ask
  ctx.tyCtx
    # unwrap
    # List.findMap
        ( \(x' /\ ty) -> do
            guard $ x == x'
            pure ty
        )
    # maybe
        ( throwError $ Err
            { message: "Unrecognized variable find a variable " <> prettyVar x <> " in context " <> prettyTyCtx ctx.tyCtx
            , subject: VarTm { var: x } an
            }
        )
        pure
