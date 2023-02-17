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
  stdout    : String
  stderr    : String
  exception : Option String
  deriving Repr


/--
  Information about a testcase.

  This structure contains meta data gathered during collection and the testrunner
  with fixture setup and teardown.
-/
structure TestcaseInfo where
  doc  : Option String
  run  : IO TestResult


/--
  Result type for captureResult to distinguish between exceptions and results.
-/
inductive ResultType (α : Type) where
  | result (r : α)
  | exception (msg : String)

namespace ResultType

  def result! [Inhabited α] (r : ResultType α) : α :=
    match r with
    | .result r => r
    | _ => panic! "not a valid result"

end ResultType


open ResultType in
/--
  Capture stdout, stderr and exceptions.

  This is taken from withIsolatedStreams in Lean/System/IO.lean and
  modified to split stdout and stderr.
-/
def captureResult {α : Type} (x : IO α) : IO (String × String × ResultType α) := do
    let bIn  ← IO.mkRef { : IO.FS.Stream.Buffer }
    let bOut ← IO.mkRef { : IO.FS.Stream.Buffer }
    let bErr ← IO.mkRef { : IO.FS.Stream.Buffer }

    let mut r := exception "unknown exception"
    try
      r := result (← IO.withStdin  (IO.FS.Stream.ofBuffer bIn)  <|
                     IO.withStdout (IO.FS.Stream.ofBuffer bOut) <|
                     IO.withStderr (IO.FS.Stream.ofBuffer bErr) x)
    catch exc =>
      r := exception exc.toString

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
    match result.exception with
    | none   => IO.println s!"[PASS] {name}"
    | some e => IO.println s!"[FAIL] {name}: {e}"
                exitcode := 1

  return exitcode

end LTest
