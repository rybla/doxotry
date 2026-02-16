module Test.Doxotry.Language.Execution where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Writer (runWriterT)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Identity (Identity)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Doxotry.Language.Common (prettyLog)
import Doxotry.Language.Execution (GenerateType, mkCtx, mkEnv, norm)
import Doxotry.Language.Grammar (Tm, Tm_(..), Ty(..), TyBase(..), generate, number, numberTy, prettyTm, ref, string, stringTy, (&), (&->), (&:), (&=>))
import Doxotry.Language.Typing as Typing
import Doxotry.Utility (runIdentity)
import Effect.Exception (error)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Execution" do
  describe "norm" do
    it_norms true stringTy
      (string "hello world")
      (string "hello world")
    it_norms true numberTy
      (number 101.0)
      (number 101.0)
    it_norms true stringTy
      (([ "x" &: stringTy ] &=> string "hello world") & [ string "ignore this" ])
      (string "hello world")
    it_norms true stringTy
      (([ "x" &: stringTy ] &=> ref "x") & [ string "hello world" ])
      (string "hello world")
    it_norms true stringTy
      (generate & [ string "" ])
      (defaultTm stringTy # runIdentity)
    it_norms true numberTy
      (generate & [ string "" ])
      (defaultTm numberTy # runIdentity)
    it_norms true ([ "x" &: stringTy ] &-> numberTy)
      (generate & [ string "" ])
      (defaultTm (([ "x" &: stringTy ] &-> numberTy)) # runIdentity)

it_norms
  :: Boolean
  -> Ty
  -> Tm ()
  -> Tm ()
  -> Spec Unit
it_norms success ty tm tm_expected =
  it ((if success then "[✅] " else "[❌] ") <> prettyTm tm <> " ===> " <> prettyTm tm_expected) do
    tm' :: Typing.TypedTm () <-
      Typing.typecheckTm ty tm
        # flip runReaderT (Typing.mkCtx {})
        # runExceptT
        # runWriterT
        # (unwrap :: Identity _ -> _)
        # case _ of
            Right tm' /\ _ -> pure tm'
            Left err /\ logs -> throwError $ error $ show err <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
    norm tm'
      # flip runReaderT
          ( mkCtx
              { defaultAn: {}
              , generate: generateImpl
              }
          )
      # flip evalStateT (mkEnv {})
      # runExceptT
      # runWriterT
      >>= case _ of
        Right tm'' /\ logs
          | success -> Typing.erase tm'' `shouldEqual` tm_expected
          | otherwise -> throwError $ error $ "norms" <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
        Left err /\ logs
          | success -> throwError $ error $ show err <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
          | otherwise -> pure unit

generateImpl :: GenerateType
generateImpl args = defaultTm args.ty

defaultTm :: forall m. Monad m => Ty -> m (Tm ())
defaultTm (BaseTy { base: NumberTyBase }) = pure $ number 0.0
defaultTm (BaseTy { base: StringTyBase }) = pure $ string "hello world"
defaultTm (ArrTy { prm, dom, cod }) = do
  body <- defaultTm cod
  pure $ LamTm { prm, dom, body } {}

