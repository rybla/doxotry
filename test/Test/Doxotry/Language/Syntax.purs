module Test.Doxotry.Language.Syntax where

import Prelude

import Doxotry.Language.Syntax (ref, stringTy, (&), (&->), (&:), (&=>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Syntax" do
  describe "elaboration" do
    "0-arity arrow type" `it` do
      shouldEqual
        ([] &-> stringTy)
        stringTy
    "2-arity arrow type" `it` do
      shouldEqual
        ([ "x" &: stringTy, "y" &: stringTy ] &-> stringTy)
        ([ "x" &: stringTy ] &-> [ "y" &: stringTy ] &-> stringTy)
    "0-arity lambda term" `it` do
      shouldEqual
        ([] &=> ref "x")
        (ref "x")
    "2-arity lambda term" `it` do
      shouldEqual
        ([ "x" &: stringTy, "y" &: stringTy ] &=> ref "x")
        ([ "x" &: stringTy ] &=> [ "y" &: stringTy ] &=> ref "x")
    "0-arity application term" `it` do
      shouldEqual
        (ref "f" & [])
        (ref "f")
    "2-arity application term" `it` do
      shouldEqual
        (ref "f" & [ ref "x", ref "y" ])
        ((ref "f" & [ ref "x" ]) & [ ref "y" ])
