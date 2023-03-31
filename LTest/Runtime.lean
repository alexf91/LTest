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

import LTest.Types
import Lean
import Cli
open Lean
open Cli

set_option relaxedAutoImplicit false

namespace LTest

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


/-- Write test results as JSON to a file. -/
def createJsonResult (results : List (Name × TestResult)) : Json := Id.run do
  return Json.null


/-- Testrunner called by the command line parser. -/
def runTests (testcases : List (Name × TestcaseInfo)) (p : Parsed) : IO UInt32 := do
  -- Print available tests and exit.
  if p.hasFlag "list-tests" then
    for (name, _) in testcases do
      IO.println name
    return 0

  let mut exitcode : UInt32 := 0
  let mut results : List (Name × TestResult) := []

  for (name, info) in testcases do
    let result ← info.run
    results := results ++ [(name, result)]

    if result.isError then
      exitcode := 1
      IO.println s!"[ERROR] {name}"

      if !result.setupErrors.isEmpty then
        IO.println "setup error:"
        for (name, error) in result.setupErrors do
          IO.println s!"{name}: {error.error!}"

      if !result.teardownErrors.isEmpty then
        IO.println "teardown errors:"
        for (name, error) in result.teardownErrors do
          IO.println s!"{name}: {error.error!}"

    else if result.isFailure then
      exitcode := 1
      IO.println s!"[FAIL] {name}: {result.testcaseResult.get!.error!}"

    else
      IO.println s!"[PASS] {name}"

  -- Write the JSON output.
  if let some path := p.flag? "json-output" then
    let r := createJsonResult results
    IO.FS.writeFile path.value r.pretty

  return exitcode


/-- Parser for the test runner. -/
private def parseCommand (testcases : List (Name × TestcaseInfo)) : Cmd := `[Cli|
  "<executable>" VIA runTests testcases;
  "Execute tests and report results."

  FLAGS:
    "json-output" : String; "Write test results to a JSON file"
    "list-tests";           "Show all available tests and exit"
]

/--
  Prototype of the main function.

  The first few arguments are set by the `#LTestMain` command and the compiler then
  uses the remaining function as entry point.
-/
def main (names : List Name) (infos : List TestcaseInfo) (args : List String) : IO UInt32 := do
  let testcases := List.zip names infos
  parseCommand testcases |>.validate args

end LTest
