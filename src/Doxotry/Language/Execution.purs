module Doxotry.Language.Execution where

import Prelude

import Data.Foldable (intercalate)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (class MonadState, modify)
import Control.Monad.Writer (class MonadWriter)
import Data.List (List, find)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Doxotry.Language.Common (Log)
import Doxotry.Language.Grammar (SemTm, SemTm_(..), Tm_(..), Ty(..), Var(..), getAnOfTm, prettyVar)
import Doxotry.Language.Typing (TypedAn, TypedTm)
import Prim.Row (class Lacks)
import Record as Record
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

type Ctx an = Ctx_ (Record an)
type Ctx_ an = { defaultAn :: an }

mkCtx
  :: forall an
   . { defaultAn :: an
     }
  -> Ctx_ an
mkCtx
  { defaultAn
  } =
  { defaultAn
  }

type Env = { freshCounter :: Int }

mkEnv :: {} -> Env
mkEnv {} =
  { freshCounter: 0
  }

newtype Err = Err { message :: String }

instance Show Err where
  show (Err err) = "Execution error: " <> err.message

--------------------------------------------------------------------------------

type TypedSemTm m an = SemTm m (TypedAn an)

type Subst m an = List (Var /\ TypedSemTm m an)

getSubst :: forall m an. MonadThrow Err m => Var -> Subst m an -> m (TypedSemTm m an)
getSubst x sigma = case sigma # find (\(x' /\ _) -> x == x') of
  Nothing -> throwError $ Err { message: "Unrecognized variable " <> prettyVar x <> " in substitution of variables " <> "[" <> (sigma # map (fst >>> prettyVar) # intercalate ", ") <> "]" }
  Just (_ /\ a) -> pure a

--------------------------------------------------------------------------------

reflect
  :: forall m an
   . MonadReader (Ctx an) m
  => MonadState Env m
  => MonadThrow Err m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => TypedTm an
  -> TypedSemTm m an
reflect tm | FunTy ty <- (tm # getAnOfTm).ty = do
  FunSemTm
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
  :: forall m an
   . MonadReader (Ctx an) m
  => MonadState Env m
  => MonadThrow Err m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => TypedSemTm m an
  -> m (TypedTm an)
reify (FunSemTm tm an) = do
  ty <- case an.ty of
    FunTy ty -> pure ty
    ty -> throwError $ Err { message: "cannot reify semantic term since it was reflected as a function term but it is annotated with the non-function type " <> show ty }
  prm <- fresh (tm.prm # unwrap).name
  body <-
    reify =<<
      tm.run
        ( reflect $
            VarTm
              { var: prm }
              (an # Record.set (Proxy @"ty") ty.dom)

        )
  pure $ LamTm { prm, dom: ty.dom, body } an
reify (SynSemTm tm) = pure tm

--------------------------------------------------------------------------------

denote
  :: forall m an
   . MonadReader (Ctx an) m
  => MonadState Env m
  => MonadThrow Err m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => Subst m an
  -> TypedTm an
  -> m (TypedSemTm m an)
denote sigma (VarTm tm _) = sigma # getSubst tm.var
denote sigma (LamTm tm an) = pure $ FunSemTm
  { prm: tm.prm
  , run: \a -> denote (sigma # List.Cons (tm.prm /\ a)) tm.body
  }
  an
denote sigma (AppTm tm _) = do
  apl <- denote sigma tm.apl >>= case _ of
    FunSemTm apl _ -> pure apl
    _ -> throwError $ Err { message: "TODO" }
  apl.run =<< denote sigma tm.arg
denote _ tm0 = pure $ SynSemTm tm0

--------------------------------------------------------------------------------

norm
  :: forall an m
   . MonadReader (Ctx an) m
  => MonadState Env m
  => MonadThrow Err m
  => MonadWriter (Array Log) m
  => Lacks "ty" an
  => TypedTm an
  -> m (TypedTm an)
norm = denote none >=> reify

--------------------------------------------------------------------------------

fresh :: forall m. MonadState Env m => String -> m Var
fresh name = do
  { freshCounter: index } <- modify (\env -> env { freshCounter = env.freshCounter + 1 })
  pure $ Var { name, mb_index: Just index }
