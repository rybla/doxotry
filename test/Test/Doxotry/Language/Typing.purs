module Test.Doxotry.Language.Typing where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Doxotry.Language.Grammar (Tm_, Ty, getAnOfTm, mkNumberLitTm, mkNumberTy, mkStringLitTm, mkStringTy, prettyTm, prettyTy)
import Doxotry.Language.Typing (checkTm, mkCtx)
import Prim.Row (class Lacks)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = describe "Typing" do
  describe "check" do
    it_checks true
      mkStringTy
      (mkStringLitTm "hello world")
    it_checks true
      mkNumberTy
      (mkNumberLitTm 100.0)
    it_checks false
      mkNumberTy
      (mkStringLitTm "hello world")

it_checks
  :: forall an
   . Lacks "ty" an
  => Show (Record an)
  => Boolean
  -> Ty
  -> Tm_ (Record an)
  -> Spec Unit
it_checks success ty tm =
  it ((if success then "yes " else "not ") <> prettyTm tm <> " : " <> prettyTy ty) do
    checkTm ty tm
      # flip runReaderT mkCtx
      # runExceptT
      # (unwrap :: Identity _ -> _)
      # map (getAnOfTm >>> _.ty)
      # case _ of
          Right _ -> unless success do fail $ "well-typed"
          Left err -> when success do fail $ show err
