module Doxotry.Language.Execution where

import Prelude

import Doxotry.Language.Grammar (SemTm, SemTm_(..), TmVar(..), Tm_(..), Ty(..), getAnOfTm)
import Doxotry.Language.Typing (TypedAn, TypedTm)

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.State (class MonadState, modify)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Prim.Row (class Lacks)
import Record as Record
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------

type Ctx an = { defaultAn :: Record an }

type Env = { freshVarCounter :: Int }

newtype Err = Err { message :: String }

--------------------------------------------------------------------------------

type TypedSemTm m an = SemTm m (TypedAn an)

--------------------------------------------------------------------------------

reflectTm
  :: forall m an
   . MonadReader (Ctx an) m
  => MonadState Env m
  => MonadThrow Err m
  => Lacks "ty" an
  => TypedTm an
  -> TypedSemTm m an
reflectTm tm | FunTy ty <- (tm # getAnOfTm).ty = do
  FunSemTm
    { name: (ty.prm # unwrap).name
    , run: \prm -> do
        arg <- reifyTm prm
        pure $ reflectTm $
          AppTm
            { apl: tm, arg }
            (getAnOfTm tm # Record.set (Proxy @"ty") ty.cod)
    }
    (getAnOfTm tm)
reflectTm tm = SynSemTm tm

reifyTm
  :: forall m (an :: Row Type)
   . MonadReader (Ctx an) m
  => MonadState Env m
  => MonadThrow Err m
  => Lacks "ty" an
  => TypedSemTm m an
  -> m (TypedTm an)
reifyTm (FunSemTm tm an) = do
  ty <- case an.ty of
    FunTy ty -> pure ty
    ty -> throwError $ Err { message: "cannot reify semantic term since it was reflected as a function term but it is annotated with the non-function type " <> show ty }
  prm <- freshVarTm tm.name
  body <-
    reifyTm =<<
      tm.run
        ( reflectTm $
            VarTm
              { var: prm }
              (an # Record.set (Proxy @"ty") ty.dom)

        )
  pure $ FunTm { prm, dom: ty.dom, body } an
reifyTm (SynSemTm tm) = pure tm

freshVarTm :: forall m. MonadState Env m => String -> m TmVar
freshVarTm name = do
  { freshVarCounter: index } <- modify (\env -> env { freshVarCounter = env.freshVarCounter + 1 })
  pure $ TmVar { name, mb_index: Just index }
