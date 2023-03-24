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
open Lean

set_option relaxedAutoImplicit false

namespace LTest


/--
  Fixture with a state of type `σ` and a value of type `α`.

  The state is updated by the `setup` and `teardown` functions, the value is passed
  to a testcase or another fixture that depends on the fixture.
-/
structure FixtureInfo (σ : Type) (α : Type) where
  doc      : Option String := none
  default  : σ
  setup    : StateT σ IO α
  teardown : StateT σ IO Unit


/--
  Result of a testcase.

  This includes the the result of the test itself and everything from
  fixture setup and teardown
-/
structure TestResult where
  stdout         : String
  stderr         : String
  testcaseError  : Option IO.Error
  setupErrors    : List IO.Error
  teardownErrors : List IO.Error

namespace TestResult
  def failure (r : TestResult) : Bool := !r.testcaseError.isNone
  def error   (r : TestResult) : Bool := !r.setupErrors.isEmpty || !r.teardownErrors.isEmpty
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
  Capture stdout, stderr and exceptions.

  This is taken from withIsolatedStreams in Lean/System/IO.lean and
  modified to split stdout and stderr.
-/
def captureResult {α : Type} (x : IO α) : IO (String × String × α) := do
    let bIn  ← IO.mkRef { : IO.FS.Stream.Buffer }
    let bOut ← IO.mkRef { : IO.FS.Stream.Buffer }
    let bErr ← IO.mkRef { : IO.FS.Stream.Buffer }

    let r := (← IO.withStdin  (IO.FS.Stream.ofBuffer bIn)  <|
            IO.withStdout (IO.FS.Stream.ofBuffer bOut) <|
            IO.withStderr (IO.FS.Stream.ofBuffer bErr) x)

    let bOut ← liftM (m := BaseIO) bOut.get
    let bErr ← liftM (m := BaseIO) bErr.get
    let out := String.fromUTF8Unchecked bOut.data
    let err := String.fromUTF8Unchecked bErr.data

    return (out, err, r)


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

    if result.error then
      exitcode := 1
      IO.println s!"[ERROR] {name}"
      if !result.setupErrors.isEmpty then
        IO.println "setup errors:"
        for error in result.setupErrors do
          IO.println s!"{error}"

      if !result.teardownErrors.isEmpty then
        IO.println "teardown errors:"
        for error in result.teardownErrors do
          IO.println s!"{error}"

    else if result.failure then
      exitcode := 1
      IO.println s!"[FAIL] {name}: {result.testcaseError.get!}"

    else
      IO.println s!"[PASS] {name}"

  return exitcode

end LTest
