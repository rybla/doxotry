module Test.Doxotry.Language.Typing where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Doxotry.Language.Common (prettyLog)
import Doxotry.Language.Grammar (Tm_, Ty, prettyTm, prettyTy)
import Doxotry.Language.Syntax (mkAppTm, mkArrTy, mkLamTm, mkNumberTm, mkNumberTy, mkStringTm, mkStringTy, mkVarTm)
import Doxotry.Language.Typing (checkTm, mkCtx)
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
      (mkArrTy "x" mkStringTy mkStringTy)
      (mkLamTm "x" mkStringTy (mkVarTm "x"))
    it_checks false
      (mkArrTy "x" mkStringTy mkNumberTy)
      (mkLamTm "x" mkStringTy (mkVarTm "x"))
    it_checks false
      (mkArrTy "x" mkNumberTy mkStringTy)
      (mkLamTm "x" mkStringTy (mkVarTm "x"))
    it_checks true
      mkStringTy
      (mkAppTm (mkLamTm "x" mkStringTy (mkVarTm "x")) (mkStringTm "hello world"))
    it_checks false
      mkStringTy
      ( mkAppTm
          ( mkAppTm
              ( mkLamTm "x" mkStringTy
                  $ mkLamTm "y" mkStringTy
                  $ mkVarTm "x"
              ) $
              (mkStringTm "hello")
          )
          (mkStringTm "world")
      )

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
          (Right _ /\ logs) -> unless success do fail $ "well-typed" <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
          (Left err /\ logs) -> when success do fail $ show err <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
