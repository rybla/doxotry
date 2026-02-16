module Test.Doxotry.Language.Execution where

import Prelude

import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.Identity.Trans (runIdentityT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Class (class MonadTrans)
import Control.Monad.Writer (runWriterT)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Doxotry.Language.Common (prettyLog)
import Doxotry.Language.Execution (Generate(..), mkCtx, mkEnv, norm)
import Doxotry.Language.Grammar (Tm, Tm_(..), Ty(..), TyBase(..), generate, lam, number, numberTy, prettyTm, rec, recTy, ref, string, stringTy, (&), (&->), (&=>))
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
      (([ "x" /\ stringTy ] &=> string "hello world") & [ string "ignore this" ])
      (string "hello world")
    it_norms true stringTy
      (([ "x" /\ stringTy ] &=> ref "x") & [ string "hello world" ])
      (string "hello world")
    it_norms true stringTy
      (generate & [ string "" ])
      (defaultTm stringTy # runIdentity)
    it_norms true numberTy
      (generate & [ string "" ])
      (defaultTm numberTy # runIdentity)
    it_norms true ([ "x" /\ stringTy ] &-> numberTy)
      (generate & [ string "" ])
      (defaultTm ([ "x" /\ stringTy ] &-> numberTy) # runIdentity)
    it_norms true (recTy [ "x" /\ stringTy ])
      (generate & [ string "" ])
      (defaultTm (recTy [ "x" /\ stringTy ]) # runIdentity)
    it_norms true (recTy [ "x" /\ stringTy ])
      (rec [ "x" /\ ((lam "s" stringTy (ref "s")) & [ string "hello world" ]) ])
      (rec [ "x" /\ string "hello world" ])

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
        # runIdentity
        # case _ of
            Right tm' /\ _ -> pure tm'
            Left err /\ logs -> throwError $ error $ show err <> "\n\n" <> "logs:\n" <> (logs # map prettyLog # intercalate "\n")
    norm tm'
      # runIdentityT
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

generateImpl :: forall t. MonadTrans t => Generate t
generateImpl = Generate \args -> defaultTm args.ty

defaultTm :: forall m. Monad m => Ty -> m (Tm ())
defaultTm (BaseTy { base: NumberTyBase }) = pure $ number 0.0
defaultTm (BaseTy { base: StringTyBase }) = pure $ string "hello world"
defaultTm (ArrTy ty) = do
  body <- defaultTm ty.cod
  pure $ LamTm { prm: ty.prm, dom: ty.dom, body } {}
defaultTm (RecTy ty) = do
  fields <- ty.fields # traverse defaultTm
  pure $ RecTm { fields } {}
