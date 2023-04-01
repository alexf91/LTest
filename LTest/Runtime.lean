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
import LTest.Instances
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
def captureResult {α : Type} (f : IO α) : IO (Except IO.Error α × Streams) := do
    let bIn  ← IO.mkRef { : IO.FS.Stream.Buffer }
    let bOut ← IO.mkRef { : IO.FS.Stream.Buffer }
    let bErr ← IO.mkRef { : IO.FS.Stream.Buffer }

    try
      let r := (← IO.withStdin  (IO.FS.Stream.ofBuffer bIn)  <|
                  IO.withStdout (IO.FS.Stream.ofBuffer bOut) <|
                  IO.withStderr (IO.FS.Stream.ofBuffer bErr) f)
      return (.ok r, .mk (← bOut.get).data (← bErr.get).data)
    catch err =>
      return (.error err, .mk (← bOut.get).data (← bErr.get).data)


/-- Filter out tests by name. -/
def filterTests (testcases : List (Name × TestcaseInfo)) (pattern : String) :=
  testcases.filter fun (n, _) => f n
where
  f (n : Name) : Bool := (n.toString.splitOn pattern |>.length) > 1


/-- Testrunner called by the command line parser. -/
def runTests (testcases : List (Name × TestcaseInfo)) (p : Parsed) : IO UInt32 := do
  -- Filter testcases if the flag is specified.
  let testcases := match p.flag? "filter" with
    | some flag => filterTests testcases flag.value
    | _         => testcases

  -- Print available tests and exit.
  if p.hasFlag "collect-only" then
    for (name, _) in testcases do
      IO.println name
    return 0

  let mut exitcode : UInt32 := 0
  let mut results : List (Name × TestResult) := []

  if p.hasFlag "verbose" then
    IO.eprintln "verbose flag not implemented"
    return 1

  for (name, info) in testcases do
    let result ← info.run
    results := results ++ [(name, result)]

    if result.type == .error then
      exitcode := 1
      IO.println s!"[ERROR] {name}"

      if !result.setupErrors.isEmpty then
        IO.println "setup error:"
        for (name, (err, streams)) in result.setupErrors do
          IO.println s!"{name}: {err}"

      if !result.teardownErrors.isEmpty then
        IO.println "teardown errors:"
        for (name, (err, streams)) in result.teardownErrors do
          IO.println s!"{name}: {err}"

      if p.hasFlag "exitfirst" then
        break

    else if result.type == .failure then
      exitcode := 1
      IO.println s!"[FAIL] {name}: {result.testcaseResult.get!.error!.1}"

      if p.hasFlag "exitfirst" then
        break

    else
      IO.println s!"[PASS] {name}"

  -- Write the JSON output.
  if let some path := p.flag? "json-output" then
    let r := toJson results
    IO.FS.writeFile path.value r.pretty

  return exitcode


/-- Parser for the test runner. -/
private def parseCommand (testcases : List (Name × TestcaseInfo)) : Cmd := `[Cli|
  "<executable>" VIA runTests testcases;
  "Execute tests and report results."

  FLAGS:
    "json-output" : String; "Write test results to a JSON file"
    "collect-only";         "Only collect tests, don't execute them"
    "x", "exitfirst";       "Exit instantly on first error or failed test"
    "v", "verbose";         "Increase verbosity"
    "k", "filter" : String; "Only run tests which contain the given substring. " ++
                            "The match is performed on the full dotted name."
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
