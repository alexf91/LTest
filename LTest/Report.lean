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

import LTest.Instances
import Lean
open Lean (Name)

set_option relaxedAutoImplicit false

namespace LTest.Report

/-- Get the number of columns in the terminal. -/
def columns : IO Nat := do
  try
    let out ← IO.Process.run {cmd := "tput", args := #["cols"]}
    match out.trim.toNat? with
    | some n => return n
    | _      => return 80
  catch _ =>
    return 80

/-- Center the string `s` in the terminal and pad it with `c`. -/
def center (s : String) (c : Char := ' ') : IO String := do
  let cols ← columns
  let lp := (cols - s.length) / 2
  let rp := (cols - s.length - lp)
  return (c.toString * lp) ++ s ++ (c.toString * rp)

/-- Shorten a string to the number of columns in the terminal. -/
def shorten (s : String) : IO String := do
  let cols ← columns
  if s.length <= cols then
    return s
  return (s.take (cols - 3) |>.dropRightWhile fun c => c.isWhitespace) ++ "..."


/-- Format a capture error for reporting. -/
private def formatCaptureError (e : CaptureError) : IO String := do
  let (err, streams) := e
  let mut out := ""
  out := out ++ s!"Exception: {err}\n\n"
  unless streams.stdout.isEmpty do
    out := out ++ (← center s!" captured stdout " '-') ++ "\n"
    out := out ++ String.fromUTF8Unchecked streams.stdout ++ "\n"
  unless streams.stderr.isEmpty do
    out := out ++ (← center s!" captured stderr " '-') ++ "\n"
    out := out ++ String.fromUTF8Unchecked streams.stderr ++ "\n"
  return out


/--
  Formatting hooks for testcase results.

  This follows the output style used by `pytest`. Output is split into three sections:
  • Short live view
  • Captured output for failures and errors
  • Summary for failures and errors

  Live output is printed after a test has been executed and is stateful to allow grouping
  tests by namespace.
-/
structure Formatter where
  /-- Header for the live section of the report. -/
  liveHeader : IO String := center " test session starts " '='

  /-- Separator string printed between two namespaces. -/
  liveNewPrefixSep : IO String := do return ""

  /-- Called when the namespace changes. The argument is the prefix of the testcase. --/
  liveNewPrefix (name : Name) : IO String

  /-- Called for every testcase before it is executed. -/
  liveResultStart (name : Name) : IO String := do return ""

  /--
    Called for every testcase after it is executed. The first argument is the fully qualified name
    of the testcase and the second argument is the result of the testcase.
  -/
  liveResultFinish (name : Name) (result : TestResult) : IO String

  /-- Footer after the live result. Probably most useful for newline handling. -/
  liveFooter : IO String

  /-- Reports for failures or errors. -/
  captures (results : List (Name × TestResult)) : IO String := do
    let errors   := results.filter (fun (_, r) => r.type == .error)
    let failures := results.filter (fun (_, r) => r.type == .failure)
    let mut out := ""

    unless errors.isEmpty do
      out := out ++ (← center " ERRORS " '=')
      for (name, result) in errors do
        for (fixture, e) in result.setupErrors do
          out := out ++ (← center s!" error in setup of {fixture} for {name} " '_') ++ "\n"
          out := out ++ (← formatCaptureError e)

        for (fixture, e) in result.teardownErrors do
          out := out ++ (← center s!" error in teardown of {fixture} for {name} " '_') ++ "\n"
          out := out ++ (← formatCaptureError e)

    unless failures.isEmpty do
      out := out ++ (← center " FAILURES " '=')
      for (name, result) in failures do
        assert! result.setupErrors.isEmpty && result.teardownErrors.isEmpty
        out := out ++ (← center s!" {name} " '_') ++ "\n"
        out := out ++ (← formatCaptureError result.testcaseResult.get!.error!)

    return out

  /-- Show a final summary of everything that happened before. -/
  summary (results : List (Name × TestResult)) : IO String := do
    if results.all fun (_, r) => r.type == .success then
      return ""

    let failures := results.filter (fun (_, r) => r.type == .failure)
    let errors   := results.filter (fun (_, r) => r.type == .error)

    let mut out ← center " short test summary info " '='
    for (name, result) in failures do
      let e := result.testcaseResult.get!.error!.1
      out := out ++ s!"{result.type.toLongString} {name} - {e}\n"

    for (name, result) in errors do
      -- Return the first exception that happens during execution.
      let e := (result.setupErrors ++ result.teardownErrors).findSome? fun (_, e) => some e.1
      out := out ++ s!"{result.type.toLongString} {name} - {e.get!}\n"

    return out

  /-- Final footer printed right before exiting. -/
  finalFooter (results : List (Name × TestResult)) : IO String := do
    let failures := results.filter (fun (_, r) => r.type == .failure) |>.length
    let errors := results.filter   (fun (_, r) => r.type == .error)   |>.length
    let success := results.filter  (fun (_, r) => r.type == .success) |>.length
    center s!" {failures} failed, {success} passed, {errors} errors " '='


/-- Formatter for verbose output. -/
def VerboseFormatter : Formatter := {
  liveNewPrefix := fun _ => do return ""
  liveResultStart := fun name => do
    return s!"{name}"
  liveResultFinish := fun _ result => do
    return s!" {result.type.toLongString}\n"
  liveFooter := do return ""
}

/-- Formatter for short output. -/
def ShortFormatter : Formatter := {
  liveNewPrefixSep := do return "\n"
  liveNewPrefix := fun name => do
    return s!"{name} "
  liveResultFinish := fun _ result => do
    return s!"{result.type.toShortString}"
  liveFooter := do return "\n"
}

end LTest.Report
