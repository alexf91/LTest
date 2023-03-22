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

import LTest.Runtime
import LTest.Extension
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
def createTestRunner (fixtures : List (Name × Name)) (body : TSyntax `term) : MacroM (TSyntax `term) := do
  let testBody ← createTestBody body
  let testRunner ← createTestHarness fixtures testBody
  return ← `(
    do
      let mut ($testcaseError) : Option IO.Error := none
      let mut ($fixtureErrors) : List IO.Error := []
      $testRunner
      return ($testcaseError, $fixtureErrors)
  )
where
  testcaseError := mkIdent (`testcaseError |>.appendAfter "✝")
  fixtureErrors := mkIdent (`fixtureErrors |>.appendAfter "✝")
  createTestHarness fixtures body := do
    match fixtures with
    | [] =>
      return body
    | (name, fixture)::fs =>
      let body ← createTestHarness fs body

      let state    := mkIdent (name.appendAfter "'✝")
      let name     := mkIdent name
      let setup    := mkIdent (fixture ++ `setup)
      let teardown := mkIdent (fixture ++ `teardown)
      let default  := mkIdent (fixture ++ `default)

      return ← `(Term.doSeqItem|
        try
          let ($name, $state) := ← ($setup |>.run $default);
          $body
          discard <| ($teardown |>.run $state)
        catch err =>
          -- This is an error in the setup or teardown function of the current fixture.
          -- Everything in `body` has its own try/catch block.
          ($fixtureErrors) := $fixtureErrors ++ [err]
      )

  createTestBody body := do
    return ← `(Term.doSeqItem|
      try
        ($body)
      catch err =>
        ($testcaseError) := some err
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
    let testRunner ← createTestRunner fixtures.toList body

    -- Create the testcase runner.
    let runner ← `(
      do
        let (out, err, (testcaseError, fixtureErrors)) ← captureResult
          $testRunner

        return {
          stdout := out
          stderr := err
          testcaseError := testcaseError
          fixtureErrors := fixtureErrors
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

/--
  Check fields for errors.
-/
def checkFields (stx : TSyntax `LTest.fixtureFields) : MacroM Unit := do
  let allowed := #[`default, `setup, `teardown]
  let fields := stx.raw[0].getArgs.map fun s => s[0].getId

  -- Check for invalid fields.
  if let some invalid := fields.filter (!allowed.contains ·) |>.get? 0 then
    Macro.throwError s!"'{invalid}' is not a valid field of a fixture"

  -- Check for missing fields.
  if let some missing := allowed.filter (!fields.contains ·) |>.get? 0 then
    Macro.throwError s!"field missing from fixture: '{missing}'"

  -- Check for duplicates.
  let duplicates := allowed.filter fun a => (fields.filter (· == a) |>.size) != 1
  if let some duplicate := duplicates.get? 0 then
    Macro.throwError s!"duplicate field: '{duplicate}'"


macro (name := fixtureDecl)
  doc?:optional(docComment)                         -- Documentation
  "fixture"                                         -- Command name
  name:ident                                        -- Name of the fixture
  stateType:ident                                   -- State type of the fixture
  valueType:ident                                   -- Value type of the fixture
  fixtures?:optional("requires" fixtureDependency+) -- Fixture arguments
  " where "                                         -- Delimiter keyword
  fields:fixtureFields                              -- Fields
  : command => do

    -- Get the docstring as a term.
    let doc := match doc? with
      | none   => mkIdent `none
      | some s => Syntax.mkStrLit s.getDocString

    -- Check if all fields exist.
    checkFields fields

    -- Find the terms for the fields we need. We already ensured that they exist.
    let getField (name : Name) : TSyntax `term :=
      fields.raw[0].getArgs.findSome! fun f =>
        if f[0].getId == name then
          some $ TSyntax.mk f[1][1]
        else
          none

    let default  := getField `default
    let setup    := getField `setup
    let teardown := getField `teardown

    -- Get fixture requirements as `(Name × Name)` tuples.
    let fixtures :=  fixtures?.raw[1].getArgs.map fun arg =>
      (arg[1].getId, arg[3].getId)

    -- TODO: Support this.
    if !fixtures.isEmpty then
      Macro.throwError "fixture dependencies are not supported"

    -- Create the setup body with fixture initialization.
    --let setup ← createFixtureSetup fixtures.toList setup

    let stx ← `(
      def $name : FixtureInfo $stateType $valueType := {
        doc      := $doc
        default  := $default
        setup    := $setup
        teardown := $teardown
      }
    )
    return TSyntax.mk stx


/--
  Generate the main function and add it to the environment.

  This can be used to compile tests and run them from the command line.
-/
elab "#LTestMain" : command => do
  let env ← getEnv
  logInfo s!"Generating LTest main function in module {env.mainModule}"

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
