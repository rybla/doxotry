module Test.Doxotry.Language.Syntax where

import Prelude

import Doxotry.Language.Syntax (stringTy, (&:), (&->))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Syntax" do
  describe "elaboration" do
    it "handles nullary arrow types" do
      shouldEqual
        ([] &-> stringTy)
        stringTy
    it "handles n-ary arrow types" do
      shouldEqual
        ([ "x" &: stringTy, "y" &: stringTy ] &-> stringTy)
        ([ "x" &: stringTy ] &-> [ "y" &: stringTy ] &-> stringTy)

