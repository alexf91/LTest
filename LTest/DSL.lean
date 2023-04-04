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

import LTest.Extension
import LTest.Runtime
import LTest.Types
import Lean
import Lean.Parser
open Lean
open Lean.Meta
open Lean.Elab.Command
open Lean.Parser


set_option relaxedAutoImplicit false

namespace LTest

/--
  Basic syntax elements used for the DSL.
-/
def fixtureDependency := leading_parser
  "(" >> ident >> ":" >> ident >> ")"


/--
  Create the testrunner.

  This creates the runner for the testcase itself and the setup and teardown code of fixtures.
-/
private def mkTestRunner (fixtures : List (Name × Name))
                         (body : Term) : MacroM Term := do
  let testBody ← mkTestBody
  let testRunner ← mkTestHarness fixtures testBody
  return ← `(
    do
      let mut ($testcaseResult)  : Option CaptureResult        := none
      let mut ($setupResults)    : List (Name × CaptureResult) := []
      let mut ($teardownResults) : List (Name × CaptureResult) := []
      let mut ($teardownFuncs)   : List (Name × IO Unit)       := []
      $testRunner
      for (name, td) in $teardownFuncs do
        let (r, streams) ← captureResult td
        let tdr : CaptureResult := match r with
          | .ok _    => .ok streams
          | .error e => .error (e, streams)

        ($teardownResults) := $teardownResults ++ [(name, tdr)]
      return ($testcaseResult, $setupResults, $teardownResults)
  )
where
  testcaseResult  := mkIdent (`testcaseResult  |>.appendAfter "✝")
  setupResults    := mkIdent (`setupResults    |>.appendAfter "✝")
  teardownResults := mkIdent (`teardownResults |>.appendAfter "✝")
  teardownFuncs   := mkIdent (`teardown        |>.appendAfter "✝")
  mkTestHarness fixtures body := do
    match fixtures with
    | [] =>
      return body
    | (varName, typeName)::fs =>
      let body   ← mkTestHarness fs body
      let name  := mkIdent varName
      let setup := mkIdent (typeName ++ `setup)

      return ← `(Term.doSeqItem| do
        let r : SetupResult _ ← ($setup)
        match r with
        | .ok ($name, _, cs) =>
          $body:doSeqItem
          ($setupResults) := cs ++ $setupResults
        | .error (e, _, cs) =>
          ($setupResults) := cs ++ $setupResults

        ($teardownFuncs) := ($teardownFuncs) ++ r.teardowns
      )

  mkTestBody := do
    return ← `(Term.doSeqItem| do
      let (r, streams) ← @captureResult Unit ($body)
      ($testcaseResult) := match r with
        | .ok _    => some $ .ok streams
        | .error e => some $ .error (e, streams)
    )

/--
  Declare a testcase.

  The testcase declaration is transformed into a function that calls the appropriate fixture
  code and returns a result.
  The transformed function has type `IO TestResult`.

  We abuse some of the syntax here, as the type of fixture arguments is really a structure
  and not a Type. This makes testcase declarations look like function definitions.
  A testcase has type `IO Unit`.

  A testcase definition looks like this:
  ```
  testcase test_something requires (a : FixtureA) (b : FixtureB) := do
    return
  ```
  The fixture part can be omitted if the testcase has no dependencies.
-/
macro (name := testcaseDecl)
  doc?:optional(docComment)                         -- Documentation
  "testcase"                                        -- Command name
  name:ident                                        -- Name of the testcase
  fixtures?:optional("requires" fixtureDependency+) -- Fixture arguments
  ":=" body:term : command => do
    -- Get fixture requirements as `(Name × Name)` tuples.
    let fixtures := fixtures?.raw[1].getArgs.map fun arg =>
      (arg[1].getId, arg[3].getId)

    -- Wrap the test body in a try/catch block and then create the surrounding fixture block,
    -- also in nested try/catch blocks.
    let testRunner ← mkTestRunner fixtures.toList body

    -- Create the testcase runner.
    let runner ← `(
      do
        -- This always succeeds, so we can ignore the error case.
        -- We don't capture the result here, because it is all captured during fixture
        -- functions and testcase execution.
        let (testcaseResult, setupResults, teardownResults) ← ($testRunner)
        return {
          testcaseResult  := testcaseResult
          setupResults    := setupResults
          teardownResults := teardownResults
        }
    )
    -- Get the docstring as a term.
    let doc := match doc? with
      | none   => mkIdent `none
      | some s => Syntax.mkStrLit s.getDocString

    -- Create the final TestcaseInfo structure and register it.
    let stx ← `(
      def $name : TestcaseInfo := {
        doc := $doc
        run := $runner
      }
      _ltest_insert_testcase $name
    )
    return stx


/--
  Declare a fixture.

  Fixtures are more complicated than testcases, because they include a state,
  a setup function and a teardown function.
  It looks more like a structure definition, but with optional dependencies on other
  fixtures.

  The following code defines a fixture `FixtureC` with a state type `σ` and a value type `α`.
  It depends on `FixtureA` and `FixtureB`.

  ```
  fixture FixtureC σ α requires (a : FixtureA) (b : FixtureB) where
    default  := (object of type σ)
    setup    := do return (object of type α)
    teardown := do return
  ```
-/
def fixtureBinder := leading_parser
  ident >> Term.binderDefault

def fixtureFields := leading_parser
  manyIndent <|
    ppLine >> checkColGe >> ppGroup fixtureBinder

def fixtureType := leading_parser
  ("(" >> termParser >> ")") <|> (ident)

private def getFixtureType (stx : Syntax) : Term :=
  match stx.getNumArgs with
  | 1 => TSyntax.mk $ stx.getArg 0
  | 3 => TSyntax.mk $ stx.getArg 1
  | _ => panic! "unknown syntax type"

/--
  Check fields for errors.
-/
private def checkFixtureFields (stx : TSyntax `LTest.fixtureFields) : MacroM Unit := do
  let allowed  := #[`default, `setup, `teardown]
  let required := #[`setup]
  let fields := stx.raw[0].getArgs.map fun s => s[0].getId

  -- Check for invalid fields.
  if let some invalid := fields.filter (!allowed.contains ·) |>.get? 0 then
    Macro.throwError s!"'{invalid}' is not a valid field of a fixture"

  -- Check for missing fields.
  if let some missing := required.filter (!fields.contains ·) |>.get? 0 then
    Macro.throwError s!"field missing from fixture: '{missing}'"

  -- Check for duplicates.
  let duplicates := allowed.filter fun a => (fields.filter (· == a) |>.size) > 1
  if let some duplicate := duplicates.get? 0 then
    Macro.throwError s!"duplicate field: '{duplicate}'"


/--
  Create the setup function.

  We transform `StateT σ IO α` that we get from the definition to `IO (SetupResult α)`.
  The result contains teardown functions of all dependent fixtures.
-/
private def mkFixtureRunner (fixtures  : List (Name × Name))
                            (typeName  : Term)
                            (default   : Term)
                            (setup     : Term)
                            (teardown  : Term)
                            (stateType : Term)
                            (valueType : Term) : MacroM Term := do

  let body ← mkFixtureBody
  let harness ← mkFixtureHarness fixtures body
  return ← `(
    let ($teardownFunc) : StateT $stateType IO Unit := ($teardown)
    do
      let mut ($teardownFuncs) : List (Name × IO Unit)       := []
      let mut ($setupResults)  : List (Name × CaptureResult) := []
      $harness
  )

where
  setupResults  := mkIdent (`srs |>.appendAfter "✝")
  teardownFuncs := mkIdent (`tds |>.appendAfter "✝")
  teardownFunc  := mkIdent (`td  |>.appendAfter "✝")
  /--
    Call the setup code of fixture dependencies and assign them to variables.
  -/
  mkFixtureHarness fixtures body := do
    match fixtures with
    | [] =>
      return body
    | (name, type)::fs =>
      let name  := mkIdent name
      let setup := mkIdent $ type ++ `setup
      let body   ← mkFixtureHarness fs body

      return ← `(Term.doSeqItem| do
        let r : SetupResult _ ← ($setup)
        ($setupResults) := $setupResults ++ r.captures
        match r with
        | .ok ($name, tds, cs) =>
          ($teardownFuncs) := tds ++ $teardownFuncs
          $body
        | .error (e, tds, cs) =>
          return .error (e, (tds ++ $teardownFuncs), ($setupResults))
      )

  /--
    Innermost part of the fixture setup code.

    This runs the setup code of the current fixture and then returns.
    `$setupResults` is populated in the nested test harness and then finally returned
    in this body.
  -/
  mkFixtureBody := do
    return ← `(Term.doSeqItem|
      do
        -- TODO: Limit which variables are visible in the setup function.
        --       Currently it shows multiple inaccessible `teardowns` lists and more.
        let setup : StateT $stateType IO $valueType := $setup
        let (r, streams) ← captureResult (setup |>.run $default)
        match r with
        | .ok (v, s) =>
          let td := (discard <| $teardownFunc |>.run s)
          return .ok (
            v,
            (($typeName, td) :: $teardownFuncs),
            ($setupResults ++ [($typeName, .ok streams)])
          )
        | .error err =>
          return .error (
            err,
            $teardownFuncs,
            ($setupResults ++ [($typeName, .error (err, streams))])
          )
    )

/--
  Create a `Term` from a `Name`.
-/
private def mkNameTerm (name : Name) : MacroM Term := do
   mk name.componentsRev
where
  mk (cs : List Name) : MacroM Term := do
  match cs with
  | []    => return ← `(Name.anonymous)
  | c::cs =>
    let cc ← `(Name.mkSimple $(Syntax.mkStrLit c.toString))
    return ← `(Name.append $(← mk cs) $cc)

macro (name := fixtureDecl)
  doc?:optional(docComment)                         -- Documentation
  "fixture"                                         -- Command name
  name:ident                                        -- Name of the fixture
  stateType:fixtureType                             -- State type of the fixture
  valueType:fixtureType                             -- Value type of the fixture
  fixtures?:optional("requires" fixtureDependency+) -- Fixture arguments
  " where "                                         -- Delimiter keyword
  fields:fixtureFields                              -- Fields
  : command => do

    -- Get the docstring as a term.
    let doc := match doc? with
      | none   => mkIdent `none
      | some s => Syntax.mkStrLit s.getDocString

    -- Get the name as a term for reporting.
    let fullName := (← Macro.getCurrNamespace) ++ name.getId
    let nameTerm ← mkNameTerm fullName

    -- Check if all fields exist.
    checkFixtureFields fields

    -- Find the terms for the fields we need. We already ensured that they exist.
    let getField (name : Name) (default : Term) : Term :=
      let result := fields.raw[0].getArgs.findSome? fun f =>
        if f[0].getId == name then
          some $ TSyntax.mk f[1][1]
        else
          none
      match result with
      | some r => r
      | none => default

    let setup    := getField `setup    $ ← `(default)
    let default  := getField `default  $ ← `(default)
    let teardown := getField `teardown $ ← `(default)

    -- Get fixture requirements as `(Name × Name)` tuples.
    let fixtures :=  fixtures?.raw[1].getArgs.map fun arg =>
      (arg[1].getId, arg[3].getId)

    let σ := getFixtureType stateType
    let α := getFixtureType valueType
    let setup ← mkFixtureRunner fixtures.toList nameTerm default setup teardown σ α

    let stx ← `(
      def $name : FixtureInfo $σ $α := {
        doc   := $doc
        setup := $setup
      }
    )
    return TSyntax.mk stx

/--
  Generate the main function and add it to the environment.

  This can be used to compile tests and run them from the command line.
-/
elab "#LTestMain" : command => do
  let testcases ← getTestcases

  liftTermElabM do
    -- Create the list of qualified testcase names.
    let names ← mkListLit (mkConst ``Name) <| Array.toList <| testcases.map toExpr

    -- Create the list of TestcaseInfo structures.
    let infos ← mkListLit (mkConst ``TestcaseInfo) <| Array.toList <| ← testcases.mapM fun n => do
      let info ← getConstInfo n
      return info.value!

    -- Add the main function to the environment.
    let mainInfo ← getConstInfoDefn ``LTest.main
    let mainValue := mainInfo.value.app names |>.app infos
    let mainType ← inferType mainValue

    addAndCompile <|.defnDecl {
      mainInfo with
        name  := `main,
        value := mainValue,
        type  := mainType
    }


end LTest
