module Test.Doxotry.Language where

import Prelude

import Test.Doxotry.Language.Execution as Test.Doxotry.Language.Execution
import Test.Doxotry.Language.Syntax as Test.Doxotry.Language.Syntax
import Test.Doxotry.Language.Typing as Test.Doxotry.Language.Typing
import Test.Spec (Spec, describe)

spec :: Spec Unit
spec = describe "Language" do
  Test.Doxotry.Language.Syntax.spec
  Test.Doxotry.Language.Typing.spec
  Test.Doxotry.Language.Execution.spec
