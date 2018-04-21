module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Test.Request as Request
import Test.Response as Response
import Test.Spec.QuickCheck (QCRunnerEffects)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  Request.spec
  Response.spec
