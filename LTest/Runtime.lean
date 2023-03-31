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

  def isError (r : CaptureResult α) := match r with
    | .error _ _ _ => true
    | _ => false

  def isSuccess (r : CaptureResult α) := match r with
    | .success _ _ _ => true
    | _ => false

end CaptureResult

/--
  Capture stdout, stderr and exceptions.

  This is taken from withIsolatedStreams in Lean/System/IO.lean and
  modified to split stdout and stderr.

  We also catch exceptions here to get the output if the function fails.
-/
def captureResult {α : Type} (f : IO α) : IO (CaptureResult α) := do
    let bIn  ← IO.mkRef { : IO.FS.Stream.Buffer }
    let bOut ← IO.mkRef { : IO.FS.Stream.Buffer }
    let bErr ← IO.mkRef { : IO.FS.Stream.Buffer }

    try
      let r := (← IO.withStdin  (IO.FS.Stream.ofBuffer bIn)  <|
                  IO.withStdout (IO.FS.Stream.ofBuffer bOut) <|
                  IO.withStderr (IO.FS.Stream.ofBuffer bErr) f)
      return .success r (← bOut.get).data (← bErr.get).data
    catch err =>
      return .error err (← bOut.get).data (← bErr.get).data


/--
  Result of the transformed `setup` functions for fixtures.
-/
inductive SetupResult (α : Type) where
  | success (value     : α)
            (teardowns : List (IO Unit))
            --(captures  : List (Name × CaptureResult Unit))
  | error (error     : IO.Error)
          (teardowns : List (IO Unit))
          --(captures  : List (Name × CaptureResult Unit))


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
  stdout          : ByteArray
  stderr          : ByteArray
  testcaseResult  : CaptureResult Unit
  setupResults    : List (CaptureResult Unit)
  teardownResults : List (CaptureResult Unit)

namespace TestResult
  def isFailure (r : TestResult) : Bool := match r.testcaseResult with
    | .error _ _ _ => true
    | _            => false

  def isError (r : TestResult) : Bool :=
    r.setupResults.any (fun r => r.isError) || r.teardownResults.any (fun r => r.isError)

  def setupErrors    (r : TestResult) := r.setupResults.filter    fun r => r.isError
  def teardownErrors (r : TestResult) := r.teardownResults.filter fun r => r.isError
end TestResult


/--
  Information about a testcase.

  This structure contains meta data gathered during collection and the testrunner
  with fixture setup and teardown.
-/
structure TestcaseInfo where
  doc  : Option String
  run  : IO TestResult


/--
  Prototype of the main function.

  The first few arguments are set by the `#LTestMain` command and the compiler then
  uses the remaining function as entry point.

  TODO: Combine the names and infos list.
-/
def main (names : List Name) (infos : List TestcaseInfo) (args : List String) : IO UInt32 := do
  let testcases := List.zip names infos
  let mut exitcode : UInt32 := 0

  for (name, info) in testcases do
    let result ← info.run

    if result.isError then
      exitcode := 1
      IO.println s!"[ERROR] {name}"

      if !result.setupErrors.isEmpty then
        IO.println "setup error:"
        for error in result.setupErrors do
          IO.println s!"{error.error!}"

      if !result.teardownErrors.isEmpty then
        IO.println "teardown errors:"
        for error in result.teardownErrors do
          IO.println s!"{error.error!}"

    else if result.isFailure then
      exitcode := 1
      IO.println s!"[FAIL] {name}: {result.testcaseResult.error!}"

    else
      IO.println s!"[PASS] {name}"

  return exitcode

end LTest
