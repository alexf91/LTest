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
import LTest.Colors
import Lean
open Lean (Name)
open LTest.Color

set_option relaxedAutoImplicit false

namespace LTest

/-- Get the number of columns in the terminal. -/
-- TODO: Use something other than tput. pytest adjusts columns to 80
--       when redirection to a file is used.
private def columns : IO Nat := do
  try
    let out ← IO.Process.run {cmd := "tput", args := #["cols"]}
    match out.trim.toNat? with
    | some n => return (n - 1)
    | _      => return 80
  catch _ =>
    return 80

/-- Center the string `s` in the terminal and pad it with `c`. -/
private def center (s : String) (c : Char := ' ') : IO String := do
  let cols ← columns
  let lp := (cols - s.length) / 2
  let rp := (cols - s.length - lp)
  return (c.toString * lp) ++ s ++ (c.toString * rp)

/-- Shorten a string to the number of columns in the terminal. -/
private def shorten (s : String) : IO String := do
  let cols ← columns
  if s.length <= cols then
    return s
  return (s.take (cols - 3) |>.dropRightWhile fun c => c.isWhitespace) ++ "..."


/-- Format a capture error for reporting. -/
private def formatStreams (streams : Streams) : IO String := do
  let mut out := ""
  unless streams.stdout.isEmpty do
    out := out ++ (← center s!" captured stdout " '-') ++ "\n"
    out := out ++ String.fromUTF8Unchecked streams.stdout ++ "\n"
  unless streams.stderr.isEmpty do
    out := out ++ (← center s!" captured stderr " '-') ++ "\n"
    out := out ++ String.fromUTF8Unchecked streams.stderr ++ "\n"
  return out


/-- Format captureError for reporting. -/
private def formatCaptureError (e : CaptureError) : IO String := do
  let (err, streams) := e
  let mut out := ""
  out := out ++ s!"Exception: {err}\n\n"
  out := out ++ (← formatStreams streams)
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
  /-- Escape codes for colors. -/
  colorMap (c : Color) : String

  /-- Separator string printed between two namespaces. -/
  liveNewPrefixSep : IO String

  /-- Called when the namespace changes. The argument is the prefix of the testcase. --/
  liveNewPrefix (name : Name) : IO String

  /-- Called for every testcase before it is executed. -/
  liveResultStart (name : Name) : IO String

  /--
    Called for every testcase after it is executed. The first argument is the fully qualified name
    of the testcase and the second argument is the result of the testcase.
  -/
  liveResultEnd (result : TestResult) : IO String

  /-- Footer after the live result. Probably most useful for newline handling. -/
  liveFooter : IO String


/-- Formatter for verbose output. -/
def VerboseFormatter : Formatter := {
  colorMap         := Color.ansiEscape
  liveNewPrefixSep := do return ""
  liveNewPrefix    := fun _ => do return ""
  liveResultStart  := fun name => do return s!"{name}"
  liveResultEnd    := fun result => do return s!" {result.type.toLongString}\n"
  liveFooter       := do return ""
}

/-- Formatter for short output. -/
def ShortFormatter : Formatter := {
  colorMap         := Color.ansiEscape
  liveNewPrefixSep := do return "\n"
  liveNewPrefix    := fun name => do return s!"{name} "
  liveResultStart  := fun _ => do return ""
  liveResultEnd    := fun result => do return s!"{result.type.toShortString}"
  liveFooter       := do return "\n"
}


namespace Formatter
  /-- Header for the live section of the report. -/
  def liveHeader (fmt : Formatter) : IO String := do
    let text ← center " test session starts " '='
    return s!"{fmt.colorMap bWhite}{text}{fmt.colorMap noColor}"

  def liveResultFinish (fmt : Formatter) (result : TestResult) : IO String := do
    let text ← fmt.liveResultEnd result
    return s!"{fmt.colorMap result.type.toColor}{text}{fmt.colorMap noColor}"

  /-- Reports for failures or errors. -/
  def captures (fmt : Formatter) (showSuccess : Bool) (results : List (Name × TestResult)) : IO String := do
    let errors    := results.filter (fun (_, r) => r.type == .error)
    let failures  := results.filter (fun (_, r) => r.type == .failure)
    let successes := results.filter (fun (_, r) => r.type == .success)
    let mut out := ""

    unless successes.isEmpty || !showSuccess do
      out := out ++ (← center " SUCCESS " '=') ++ "\n"
      for (name, result) in successes do
        assert! result.setupErrors.isEmpty && result.teardownErrors.isEmpty
        let text ← center s!" {name} " '_'
        out := out ++ s!"{fmt.colorMap bGreen}{text}{fmt.colorMap noColor}" ++ "\n"
        out := out ++ (← formatStreams result.testcaseResult.get!.ok!)

    unless errors.isEmpty do
      out := out ++ (← center " ERRORS " '=') ++ "\n"
      for (name, result) in errors do
        for (fixture, e) in result.setupErrors do
          let text ← center s!" ERROR at setup of {fixture} for {name} " '_'
          out := out ++ s!"{fmt.colorMap bRed}{text}{fmt.colorMap noColor}" ++ "\n"
          out := out ++ (← formatCaptureError e)

        for (fixture, e) in result.teardownErrors do
          let text ← center s!" ERROR at teardown of {fixture} for {name} " '_'
          out := out ++ s!"{fmt.colorMap bRed}{text}{fmt.colorMap noColor}" ++ "\n"
          out := out ++ (← formatCaptureError e)

    unless failures.isEmpty do
      out := out ++ (← center " FAILURES " '=') ++ "\n"
      for (name, result) in failures do
        assert! result.setupErrors.isEmpty && result.teardownErrors.isEmpty
        let text ← center s!" {name} " '_'
        out := out ++ s!"{fmt.colorMap bRed}{text}{fmt.colorMap noColor}" ++ "\n"
        out := out ++ (← formatCaptureError result.testcaseResult.get!.error!)

    return out


  /-- Show a final summary of everything that happened before. -/
  def summary (fmt : Formatter) (results : List (Name × TestResult)) : IO String := do
    if results.all fun (_, r) => r.type == .success then
      return ""

    let failures := results.filter (fun (_, r) => r.type == .failure)
    let errors   := results.filter (fun (_, r) => r.type == .error)

    let text ← center " short test summary info " '='
    let mut out := s!"{fmt.colorMap bCyan}{text}{fmt.colorMap noColor}\n"
    for (name, result) in failures do
      let e := result.testcaseResult.get!.error!.1
      let r := s!"{fmt.colorMap result.type.toColor}{result.type.toLongString}{fmt.colorMap noColor}"
      out := out ++ s!"{r} {name} - {e}{fmt.colorMap noColor}\n"

    for (name, result) in errors do
      -- Return the first exception that happens during execution.
      let e := (result.setupErrors ++ result.teardownErrors).findSome? fun (_, e) => some e.1
      let r := s!"{fmt.colorMap result.type.toColor}{result.type.toLongString}{fmt.colorMap noColor}"
      out := out ++ s!"{r} {name} - {e.get!}\n"

    return out


  /-- Final footer printed right before exiting. -/
  def finalFooter (fmt : Formatter) (results : List (Name × TestResult)) : IO String := do
    let failures := results.filter (fun (_, r) => r.type == .failure) |>.length
    let errors := results.filter   (fun (_, r) => r.type == .error)   |>.length
    let success := results.filter  (fun (_, r) => r.type == .success) |>.length
    let text ← center s!" {failures} failed, {success} passed, {errors} errors " '='
    let color := if failures == 0 && errors == 0 then bGreen else bRed
    return s!"{fmt.colorMap color}{text}{fmt.colorMap noColor}\n"

end Formatter

end LTest
