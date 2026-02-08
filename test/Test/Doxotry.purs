module Test.Doxotry where

import Prelude

import Test.Doxotry.Language as Test.Doxotry.Language
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Doxotry" do
  Test.Doxotry.Language.spec
