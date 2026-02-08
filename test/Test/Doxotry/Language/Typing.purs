module Test.Doxotry.Language.Typing where

import Prelude

import Doxotry.Language.Grammar (Tm_, Ty, getAnOfTm, mkStringLitTm, mkStringTy)
import Doxotry.Language.Typing (checkTm, mkCtx)

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Effect.Aff as Aff
import Prim.Row (class Lacks)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = describe "Typing" do
  describe "check" do
    it_checks
      mkStringTy
      (mkStringLitTm "hello world")
    it_checks
      mkStringTy
      (mkStringLitTm "hello world")

it_checks
  :: forall an
   . Lacks "ty" an
  => Show (Record an)
  => Ty
  -> Tm_ (Record an)
  -> Spec Unit
it_checks ty tm =
  it (show tm <> " :: " <> show ty) do
    checkTm ty tm
      # flip runReaderT mkCtx
      # runExceptT
      # (unwrap :: Identity _ -> _)
      # map (getAnOfTm >>> _.ty)
      # case _ of
          Right _ -> pure unit
          Left err -> fail $ show err
