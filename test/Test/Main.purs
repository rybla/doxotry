module Test.Main where

import Prelude

import Test.Doxotry as Test.Doxotry

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  Test.Doxotry.spec
