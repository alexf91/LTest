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
open Lean
open Lean.Meta
open Lean.Elab.Command



set_option relaxedAutoImplicit false

namespace LTest

/--
  Basic syntax elements used for the DSL.
-/
syntax fixtureDependency := "(" ident ":" ident ")"


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
    dbg_trace s!"name: {name.getId}"

    -- Get fixture requirements as `(Name × Name)` tuples.
    let fixtures :=  fixtures?.raw[1].getArgs.map fun arg =>
      (arg[1].getId, arg[3].getId)
    dbg_trace s!"fixtures: {fixtures}"

    -- TODO: This only works for testcases without fixtures.

    -- Create the testcase runner.
    let runner ← `(
      do
        let (out, err, result) ← @captureResult Unit
          $body

        let exception := match result with
        | .result _    => none
        | .exception s => some s

        return {
          stdout := out
          stderr := err
          exception := exception
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
macro (name := fixtureDecl)
  doc?:optional(docComment)                         -- Documentation
  "fixture"                                         -- Command name
  name:ident                                        -- Name of the fixture
  state:ident                                       -- State type of the fixture
  value:ident                                       -- Value type of the fixture
  fixtures?:optional("requires" fixtureDependency+) -- Fixture arguments
  "where"                                           -- Delimiter keyword
  body:term : command => do
    return TSyntax.mk mkNullNode


fixture FooBar Nat Nat requires (a : FixtureA) (b : FixtureB) where
  let a := 20
  return


/--
  Generate the main function and add it to the environment.

  This can be used to compile tests and run them from the command line.
-/
elab "#LTestMain" : command => do
  let env ← getEnv
  logInfo s!"Generating LTest main function in module {env.mainModule}"

  let testcases ← getTestcases

  liftTermElabM do
    -- TODO: Populate the list.
    let empty ← mkListLit (mkConst ``TestcaseInfo) []

    -- Add the main function to the environment.
    let mainInfo ← getConstInfoDefn ``LTest.main
    let mainValue := mainInfo.value.app empty
    let mainType ← inferType mainValue

    addAndCompile <|.defnDecl {
      mainInfo with
        name  := `main,
        value := mainValue,
        type  := mainType
    }


end LTest
