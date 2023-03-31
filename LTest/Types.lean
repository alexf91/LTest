--
-- Copyright 2023 Alexander Fasching
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
--

import Lean
import Cli
open Lean

set_option relaxedAutoImplicit false

namespace LTest

/--
  Captured result.
-/
inductive CaptureResult (α : Type) where
  | success (value : α)        (stdout : ByteArray) (stderr : ByteArray)
  | error   (error : IO.Error) (stdout : ByteArray) (stderr : ByteArray)
  deriving Inhabited

namespace CaptureResult

  def error! (r : CaptureResult α) := match r with
    | .error e _ _ => e
    | _ => panic! "result does not contain error"

  def value! [Inhabited α] (r : CaptureResult α) := match r with
    | .success v _ _ => v
    | _ => panic! "result does not contain value"

  def isError (r : CaptureResult α) := match r with
    | .error _ _ _ => true
    | _ => false

  def isSuccess (r : CaptureResult α) := match r with
    | .success _ _ _ => true
    | _ => false

  /-- Return the same object, but replace the value with `()`. -/
  def withoutValue (r : CaptureResult α) : CaptureResult Unit := match r with
    | .success _ out err => .success () out err
    | .error e out err   => .error e out err

  def stdout (r : CaptureResult α) := match r with
    | .success _ out _ => out
    | .error   _ out _ => out

  def stderr (r : CaptureResult α) := match r with
    | .success _ _ err => err
    | .error   _ _ err => err

end CaptureResult


/--
  Result of the transformed `setup` functions for fixtures.
-/
inductive SetupResult (α : Type) where
  | success (value     : α)
            (teardowns : List (Name × IO Unit))
            (captures  : List (Name × CaptureResult Unit))
  | error (error     : IO.Error)
          (teardowns : List (Name × IO Unit))
          (captures  : List (Name × CaptureResult Unit))
  deriving Inhabited

namespace SetupResult

  def teardowns (r : SetupResult α) := match r with
    | .success _ tds _ => tds
    | .error   _ tds _ => tds

  def captures (r : SetupResult α) := match r with
    | .success _ _ cs => cs
    | .error   _ _ cs => cs

end SetupResult

/--
  Fixture with a state of type `σ` and a value of type `α`.

  The state is updated by the `setup` and `teardown` functions, the value is passed
  to a testcase or another fixture that depends on the fixture.
-/
structure FixtureInfo (σ : Type) (α : Type) where
  name  : Name
  doc   : Option String := none
  setup : IO (SetupResult α)


/--
  Result of a testcase.

  This includes the the result of the test itself and everything from
  fixture setup and teardown
-/
structure TestResult where
  testcaseResult  : Option (CaptureResult Unit)
  setupResults    : List (Name × CaptureResult Unit)
  teardownResults : List (Name × CaptureResult Unit)

namespace TestResult
  def isFailure (r : TestResult) : Bool := match r.testcaseResult with
    | some (.error _ _ _) => true
    | _                   => false

  def isError (r : TestResult) : Bool :=
    r.setupResults.any (fun r => r.2.isError) || r.teardownResults.any (fun r => r.2.isError)

  def setupErrors    (r : TestResult) := r.setupResults.filter    fun r => r.2.isError
  def teardownErrors (r : TestResult) := r.teardownResults.filter fun r => r.2.isError
end TestResult


/--
  Information about a testcase.

  This structure contains meta data gathered during collection and the testrunner
  with fixture setup and teardown.
-/
structure TestcaseInfo where
  doc  : Option String
  run  : IO TestResult

end LTest
