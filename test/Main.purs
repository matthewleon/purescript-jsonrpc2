module Test.Main where

import Prelude

import Effect (Effect)
import Test.Request as Request
import Test.Request.Batch as RequestBatch
import Test.Response as Response
import Test.Response.Batch as ResponseBatch
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)

main :: Effect Unit
main = run [consoleReporter] do
  Request.spec
  RequestBatch.spec
  Response.spec
  ResponseBatch.spec
