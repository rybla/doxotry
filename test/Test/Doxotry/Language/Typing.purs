module Test.Doxotry.Language.Typing where

import Doxotry.Language.Grammar
import Doxotry.Language.Typing
import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Doxotry.Language.Common (prettyLog)
import Prim.Row (class Lacks)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = describe "Typing" do
  describe "check" do
    it_checks true
      mkStringTy
      (mkStringTm "hello world")
    it_checks true
      mkNumberTy
      (mkNumberTm 100.0)
    it_checks false
      mkNumberTy
      (mkStringTm "hello world")
    it_checks true
      (mkFunTy (mkVar "x") mkStringTy mkStringTy)
      (mkFunTm (mkVar "x") mkStringTy (mkVarTm "x"))
    it_checks false
      (mkFunTy (mkVar "x") mkStringTy mkNumberTy)
      (mkFunTm (mkVar "x") mkStringTy (mkVarTm "x"))
    it_checks false
      (mkFunTy (mkVar "x") mkNumberTy mkStringTy)
      (mkFunTm (mkVar "x") mkStringTy (mkVarTm "x"))
    it_checks true
      mkStringTy
      (mkAppTm (mkFunTm (mkVar "x") mkStringTy (mkVarTm "x")) (mkStringTm "hello world"))

it_checks
  :: forall an
   . Lacks "ty" an
  => Show (Record an)
  => Boolean
  -> Ty
  -> Tm_ (Record an)
  -> Spec Unit
it_checks success ty tm =
  it ((if success then "yes " else "no  ") <> prettyTm tm <> " : " <> prettyTy ty) do
    checkTm ty tm
      # flip runReaderT mkCtx
      # runExceptT
      # runWriterT
      # (unwrap :: Identity _ -> _)
      # case _ of
          (Right tm /\ logs) -> unless success do fail $ "well-typed" <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
          (Left err /\ logs) -> when success do fail $ show err <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
