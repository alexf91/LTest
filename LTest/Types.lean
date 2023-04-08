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

import LTest.Colors
import Lean
import Cli
open Lean

set_option relaxedAutoImplicit false

namespace LTest

/-- A pair of standard out and error. -/
structure Streams where
  stdout : ByteArray
  stderr : ByteArray
  deriving Inhabited

/-- Result of the error branch of `CaptureResult`. -/
abbrev CaptureError := (IO.Error × Streams)
deriving instance Inhabited for CaptureError

/-- Result of the okay branch of `CaptureResult`. -/
abbrev CaptureOkay := Streams
deriving instance Inhabited for CaptureOkay

/-- Result type to store output captures for reporting. -/
abbrev CaptureResult := Except CaptureError CaptureOkay
deriving instance Inhabited for CaptureResult

namespace CaptureResult
  def error! (r : CaptureResult) : CaptureError := match r with
  | Except.error e => e
  | _              => panic! "result does not contain error"

  def ok! (r : CaptureResult) : CaptureOkay := match r with
  | Except.ok e => e
  | _           => panic! "result does not contain value"
end CaptureResult

/-- Result of the error branch of `SetupResult`: `(error, teardowns, captures)` -/
abbrev SetupError := (IO.Error × List (Name × IO Unit) × List (Name × CaptureResult))
deriving instance Inhabited for SetupError

/-- Result of the okay branch of `SetupResult`: `(value, teardowns, captures)` -/
abbrev SetupOkay (α : Type) := (α × List (Name × IO Unit) × List (Name × CaptureResult))
instance [Inhabited α] : Inhabited (SetupOkay α) := inferInstance

/-- Results of the transformed `setup` functions for fixtures. -/
abbrev SetupResult (α : Type) := Except SetupError (SetupOkay α)
instance [Inhabited α] : Inhabited (SetupResult α) := inferInstance

namespace SetupResult

  def teardowns (r : SetupResult α) := match r with
    | .ok    (_, tds, _) => tds
    | .error (_, tds, _) => tds

  def captures (r : SetupResult α) := match r with
    | .ok    (_, _, cs) => cs
    | .error (_, _, cs) => cs

end SetupResult

/--
  Fixture with a state of type `σ` and a value of type `α`.

  The state is updated by the `setup` and `teardown` functions, the value is passed
  to a testcase or another fixture that depends on the fixture.
-/
structure FixtureInfo (σ : Type) (α : Type) where
  doc   : Option String := none
  setup : IO (SetupResult α)


/-- Type of a test result. -/
inductive TestResultType where
  | success
  | failure
  | error
  deriving Inhabited, BEq

namespace TestResultType
  open Color in
  def toLongString (r : TestResultType) := match r with
    | .success => s!"{green}PASSED{noColor}"
    | .failure => s!"{red}FAILED{noColor}"
    | .error   => s!"{red}ERROR{noColor}"

  open Color in
  def toShortString (r : TestResultType) := match r with
    | .success => s!"."
    | .failure => s!"{red}F{noColor}"
    | .error   => s!"{red}E{noColor}"
end TestResultType

/--
  Result of a testcase.

  This includes the the result of the test itself and everything from
  fixture setup and teardown
-/
structure TestResult where
  testcaseResult  : Option CaptureResult
  setupResults    : List (Name × CaptureResult)
  teardownResults : List (Name × CaptureResult)

namespace TestResult
  def type (r : TestResult) : TestResultType :=
    let failure := match r.testcaseResult with
      | some (Except.error _) => true
      | _                     => false
    let error := r.setupResults.any (fun r => !r.2.isOk) || r.teardownResults.any (fun r => !r.2.isOk)
    match (failure, error) with
      | (_, true) => .error
      | (true, _) => .failure
      | _         => .success

  def setupErrors (r : TestResult) : List (Name × CaptureError) :=
    r.setupResults.filterMap fun (n, c) => match c with
    | Except.error e => some (n, e)
    | _              => none

  def teardownErrors (r : TestResult) : List (Name × CaptureError) :=
    r.teardownResults.filterMap fun (n, c) => match c with
    | Except.error e => some (n, e)
    | _              => none

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
