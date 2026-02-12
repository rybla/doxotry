module Test.Doxotry.Language.Syntax where

import Prelude

import Doxotry.Language.Syntax (stringTy, (&:), (&->))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Syntax" do
  describe "elaboration" do
    "handles 0-arity arrow types" `it` do
      shouldEqual
        ([] &-> stringTy)
        stringTy
    "n-arity arrow types" `it` do
      shouldEqual
        ([ "x" &: stringTy, "y" &: stringTy ] &-> stringTy)
        ([ "x" &: stringTy ] &-> [ "y" &: stringTy ] &-> stringTy)
