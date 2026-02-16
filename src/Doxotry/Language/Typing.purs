module Doxotry.Language.Typing where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadReader, ask, local)
import Control.Monad.Writer (class MonadWriter)
import Data.Array as Array
import Data.Foldable (intercalate)
import Data.List as List
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Newtype (over, unwrap)
import Data.Set as Set
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Doxotry.Language.Common (Log, tellLog)
import Doxotry.Language.Grammar (Tm, TmLit(..), Tm_(..), Ty(..), TyBase(..), TyCtx(..), Var, getDomOfTm, prettyTm, prettyTy, prettyTyCtx, prettyVar, stringTy)
import Prim.Row (class Lacks)
import Record as Record
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

type Ctx = { tyCtx :: TyCtx }

mkCtx :: {} -> Ctx
mkCtx {} =
  { tyCtx: TyCtx none
  }

type Env = {}

mkEnv :: {} -> Env
mkEnv {} =
  {}

type Err an = Err_ (Record an)
newtype Err_ an = Err
  { message :: String
  , subject :: Tm_ an
  }

instance Show an => Show (Err_ an) where
  show (Err err) = "Typing error: " <> err.message <> "\nsubject: " <> prettyTm err.subject

derive newtype instance Eq an => Eq (Err_ an)

--------------------------------------------------------------------------------

type TypedTm an = Tm (TypedAn an)
type TypedAn an = (ty :: Ty | an)

erase :: forall an. Lacks "ty" an => TypedTm an -> Tm an
erase = map (Record.delete (Proxy @"ty"))

--------------------------------------------------------------------------------

typecheckTm
  :: forall m an
   . MonadReader Ctx m
  => MonadThrow (Err an) m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
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
    throwError $ Err { message: "The var " <> prettyTm tm0 <> " was expected to have type " <> prettyTy ty <> ", but it actually has type " <> prettyTy ty', subject: tm0 }
  pure $ VarTm { var: tm.var } (Record.insert (Proxy @"ty") ty an)
-- AppTm
typecheckTm ty tm0@(AppTm tm an) = do
  log_typecheckTm ty tm0
  arr <- getDomOfTm tm.apl # flip maybe pure (throwError $ Err { message: "The applicant of an application must have a function type, but it's actually " <> prettyTm tm.apl, subject: tm0 })
  apl' <- typecheckTm (ArrTy { prm: arr.prm, dom: arr.dom, cod: ty }) tm.apl
  arg' <- typecheckTm arr.dom tm.arg
  pure $ AppTm
    { apl: apl'
    , arg: arg'
    }
    (Record.insert (Proxy @"ty") ty an)
-- LamTm
typecheckTm ty tm0@(LamTm tm an) = do
  log_typecheckTm ty tm0
  arr <- case ty of
    ArrTy arr -> pure arr
    _ -> throwError $ Err { message: "The term " <> prettyTm tm0 <> " was expected to have a non-function type " <> prettyTy ty <> ", but it is actually a function term", subject: tm0 }
  unless (arr.dom == tm.dom) do
    throwError $ Err { message: "The term " <> prettyTm tm0 <> " was expected to be a function term with domain " <> prettyTy arr.dom <> ", but it actually had domain " <> prettyTy tm.dom, subject: tm0 }
  b <-
    extendTyCtx tm.prm tm.dom do
      typecheckTm arr.cod tm.body
  pure $
    LamTm
      { prm: tm.prm, dom: tm.dom, body: b }
      (Record.insert (Proxy @"ty") ty an)
-- GenerateTm
typecheckTm ty tm0@(GenerateTm tm an) = do
  log_typecheckTm ty tm0
  arr <- case ty of
    ArrTy arr -> pure arr
    _ -> throwError $ Err { message: "The term " <> prettyTm tm0 <> " was expected to have a non-function type " <> prettyTy ty <> ", but it is actually a function term", subject: tm0 }
  unless (arr.dom == stringTy) do
    throwError $ Err { message: "The term " <> prettyTm tm0 <> " was expected to have a function type with domain " <> prettyTy arr.dom <> ", but it actually had domain " <> prettyTy stringTy, subject: tm0 }
  pure $
    GenerateTm
      tm
      (Record.insert (Proxy @"ty") ty an)
typecheckTm ty0@(RecTy ty) tm0@(RecTm tm an) = do
  log_typecheckTm ty0 tm0
  unless ((ty.fields # Map.toUnfoldable # map @Array (\(k /\ _) -> k)) == (tm.fields # Map.toUnfoldable # map @Array (\(k /\ _) -> k))) do
    let
      prettyKeys keys = keys # Set.toUnfoldable # map @Array (\(i /\ x) -> "[" <> show i <> "] " <> show x) # intercalate ", "
      missingKeys = (ty.fields # Map.keys) `Set.difference` (tm.fields # Map.keys) # prettyKeys
      extraKeys = (tm.fields # Map.keys) `Set.difference` (ty.fields # Map.keys) # prettyKeys
    throwError $ Err { message: "The record term " <> prettyTm tm0 <> " was expected to have record type " <> prettyTy ty0 <> ", but it doesn't have the expected record keys; missing keys are " <> missingKeys <> "; extra keys are " <> extraKeys, subject: tm0 }
  fields <-
    Array.zip (ty.fields # Map.toUnfoldable) (tm.fields # Map.toUnfoldable)
      # traverse \((k /\ ty') /\ (_ /\ tm')) -> do
          (k /\ _) <$> typecheckTm ty' tm'
  pure
    $ RecTm
        { fields: Map.fromFoldable fields }
        (Record.insert (Proxy @"ty") ty0 an)
-- type error
typecheckTm ty tm = do
  tellLog "typecheckTm" $ prettyTm tm <> " : " <> prettyTy ty
  throwError $ Err { message: "The term " <> prettyTm tm <> " was expected to have type " <> prettyTy ty <> ", but it can't have that type.", subject: tm }

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
    # flip maybe pure (throwError $ Err { message: "Unrecognized variable " <> prettyVar x <> " in context " <> prettyTyCtx ctx.tyCtx, subject: VarTm { var: x } an })
