module Doxotry.Language.Execution where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (class MonadReader, ask, runReaderT)
import Control.Monad.State (class MonadState, modify)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadWriter)
import Data.Either (either)
import Data.Foldable (intercalate)
import Data.List (List, find)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Doxotry.Language.Common (Log)
import Doxotry.Language.Grammar (SemTm, SemTm_(..), TmLit(..), Tm_(..), Ty(..), Var(..), Tm, getAnOfTm, prettySemTm, prettyTm, prettyTy, prettyVar, var)
import Doxotry.Language.Typing (TypedAn, TypedTm, erase)
import Doxotry.Language.Typing as Typing
import Prim.Row (class Lacks)
import Record as Record
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

type Ctx t an = Ctx_ t (Record an)

type Ctx_ t an =
  { defaultAn :: an
  , generate :: Generate t
  }

newtype Generate (t :: (Type -> Type) -> Type -> Type) = Generate (GenerateK t)

type GenerateK (t :: (Type -> Type) -> Type -> Type) = forall m. Monad (t m) => Prompt -> t m (Tm ())

runGenerate :: forall t. Generate t -> GenerateK t
runGenerate (Generate f) = f

type Prompt =
  { ty :: Ty
  , user :: String
  }

mkCtx
  :: forall t an
   . { defaultAn :: an
     , generate :: Generate t
     }
  -> Ctx_ t an
mkCtx
  { defaultAn
  , generate
  } =
  { defaultAn
  , generate
  }

type Env =
  { freshCounter :: Int
  , generationCache :: Map.Map String String
  }

mkEnv :: {} -> Env
mkEnv {} =
  { freshCounter: 0
  , generationCache: Map.empty
  }

type Err an = Err_ (Record an)
newtype Err_ an = Err
  { message :: String
  , subject :: Maybe (Tm_ an)
  }

instance Show an => Show (Err_ an) where
  show (Err err) = "Execution error: " <> err.message

--------------------------------------------------------------------------------

type TypedSemTm (t :: (Type -> Type) -> Type -> Type) m an = SemTm (t m) (TypedAn an)
type Subst (t :: (Type -> Type) -> Type -> Type) m an = List (Var /\ TypedSemTm t m an)

getSubst
  :: forall t m an
   . MonadTrans t
  => MonadThrow (Err an) m
  => Var
  -> Subst t m an
  -> t m (TypedSemTm t m an)
getSubst x sigma = case sigma # find (\(x' /\ _) -> x == x') of
  Nothing -> lift $ throwError $ Err { message: "Unrecognized variable " <> prettyVar x <> " in substitution of variables " <> "[" <> (sigma # map (fst >>> prettyVar) # intercalate ", ") <> "]", subject: none }
  Just (_ /\ a) -> lift $ pure a

--------------------------------------------------------------------------------

reflect
  :: forall t m an
   . MonadTrans t
  => Monad (t m)
  => MonadReader (Ctx t an) m
  => MonadState Env m
  => MonadThrow (Err an) m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => TypedTm an
  -> TypedSemTm t m an
reflect tm | ArrTy ty <- (tm # getAnOfTm).ty = do
  LamSemTm
    { prm: ty.prm
    , run: \prm -> do
        arg <- reify prm
        pure $ reflect $
          AppTm
            { apl: tm, arg }
            (getAnOfTm tm # Record.set (Proxy @"ty") ty.cod)
    }
    (getAnOfTm tm)
reflect tm = SynSemTm tm

--------------------------------------------------------------------------------

reify
  :: forall t m an
   . MonadTrans t
  => Monad (t m)
  => MonadReader (Ctx t an) m
  => MonadState Env m
  => MonadThrow (Err an) m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => TypedSemTm t m an
  -> t m (TypedTm an)
reify (LamSemTm tm an) = do
  ty <- case an.ty of
    ArrTy ty -> lift $ pure ty
    ty -> lift $ throwError $ Err { message: "Cannot reify semantic term since it was reflected as a semantic function term but it is annotated with the non-function type " <> show ty, subject: none }
  prm <- lift $ fresh (tm.prm # unwrap).name
  let
    arg = reflect $
      VarTm
        { var: prm }
        (an # Record.set (Proxy @"ty") ty.dom)
  body <- reify =<< tm.run arg
  pure $ LamTm { prm, dom: ty.dom, body } an
reify (SynSemTm tm) = lift $ pure tm

--------------------------------------------------------------------------------

denote
  :: forall t m an
   . MonadTrans t
  => Monad (t m)
  => MonadReader (Ctx t an) m
  => MonadState Env m
  => MonadThrow (Err an) m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => Subst t m an
  -> TypedTm an
  -> t m (TypedSemTm t m an)
denote sigma (VarTm tm _) = sigma # getSubst tm.var
denote sigma (LamTm tm an) = pure $ LamSemTm
  { prm: tm.prm
  , run: \arg -> denote (sigma # List.Cons (tm.prm /\ arg)) tm.body
  }
  an
denote sigma (AppTm tm _) = do
  apl <- denote sigma tm.apl >>= case _ of
    LamSemTm apl _ -> lift $ pure apl
    apl -> lift $ throwError $ Err { message: "A non-function semantic term was used as an applicant: " <> prettySemTm apl, subject: none }
  apl.run =<< denote sigma tm.arg
denote _sigma tm0@(GenerateTm _tm an) = pure $ LamSemTm
  { prm: var "prompt"
  , run: \prompt -> reify prompt >>= case _ of
      LitTm { lit: StringTmLit promptString } _an_prompt -> do
        ctx <- lift ask
        arr <- case an.ty of
          ArrTy arr -> lift $ pure arr
          ty -> lift $ throwError $ Err { message: "The term " <> prettyTm tm0 <> " must have a function type, but it was annotated with type " <> prettyTy ty, subject: pure $ erase tm0 }
        result <-
          runGenerate ctx.generate
            { ty: arr.cod
            , user: promptString
            }
            <#> map (const ctx.defaultAn)
        result' <-
          Typing.typecheckTm arr.cod result
            # runExceptT
            # flip runReaderT (Typing.mkCtx {})
            >>= either (\(Typing.Err err) -> throwError $ Err { message: "Error when typechecking generated term: " <> err.message, subject: pure result }) pure
            # lift
        pure $ SynSemTm $ result'
      prompt' -> lift $ throwError $ Err { message: "Cannot run generate with this term as a prompt: " <> prettyTm prompt', subject: pure $ erase tm0 }
  }
  an
denote sigma (RecTm tm an) = do
  -- TODO: this works, but is this the right place to do it?
  fields <- tm.fields # traverse (denote sigma >=> reify)
  pure $ SynSemTm $ RecTm { fields } an
denote _ tm0 = pure $ SynSemTm tm0

--------------------------------------------------------------------------------

norm
  :: forall t an m
   . MonadTrans t
  => Monad (t m)
  => MonadReader (Ctx t an) m
  => MonadState Env m
  => MonadThrow (Err an) m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => TypedTm an
  -> t m (TypedTm an)
norm = denote none >=> reify

--------------------------------------------------------------------------------

fresh :: forall m. MonadState Env m => String -> m Var
fresh name = do
  { freshCounter: index } <- modify (\env -> env { freshCounter = env.freshCounter + 1 })
  pure $ Var { name, mb_index: Just index }
