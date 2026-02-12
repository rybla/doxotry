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
import Doxotry.Language.Syntax (number, numberTy, ref, string, stringTy, (&), (&->), (&:), (&=>))
import Doxotry.Language.Typing (typecheckTm, mkCtx)
import Prim.Row (class Lacks)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail)

spec :: Spec Unit
spec = describe "Typing" do
  describe "typecheck" do
    it_typechecks true
      stringTy
      (string "hello world")
    it_typechecks true
      numberTy
      (number 100.0)
    it_typechecks false
      numberTy
      (string "hello world")
    it_typechecks true
      ([ "x" &: stringTy ] &-> stringTy)
      ([ "x" &: stringTy ] &=> ref "x")
    it_typechecks false
      ([ "x" &: stringTy ] &-> numberTy)
      ([ "x" &: stringTy ] &=> ref "x")
    it_typechecks false
      ([ "x" &: numberTy ] &-> numberTy)
      ([ "x" &: stringTy ] &=> ref "x")
    it_typechecks true
      stringTy
      (([ "x" &: stringTy ] &=> ref "x") & [ string "hello world" ])
    it_typechecks false
      stringTy
      ( ([ "x" &: stringTy, "y" &: stringTy ] &=> ref "x") &
          [ string "hello", string "world" ]
      )

it_typechecks
  :: forall an
   . Lacks "ty" an
  => Show (Record an)
  => Boolean
  -> Ty
  -> Tm_ (Record an)
  -> Spec Unit
it_typechecks success ty tm =
  it ((if success then "[✅] " else "[❌] ") <> prettyTm tm <> " : " <> prettyTy ty) do
    typecheckTm ty tm
      # flip runReaderT mkCtx
      # runExceptT
      # runWriterT
      # (unwrap :: Identity _ -> _)
      # case _ of
          (Right _ /\ logs) -> unless success do fail $ "well-typed" <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
          (Left err /\ logs) -> when success do fail $ show err <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
