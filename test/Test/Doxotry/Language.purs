module Test.Doxotry.Language where

import Prelude

import Test.Doxotry.Language.Execution as Test.Doxotry.Language.Execution
import Test.Doxotry.Language.Typing as Test.Doxotry.Language.Typing
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Language" do
  Test.Doxotry.Language.Typing.spec
  Test.Doxotry.Language.Execution.spec
