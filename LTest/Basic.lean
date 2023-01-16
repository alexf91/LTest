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
  An assertion (or a testcase) never returns a value.
  Failed assertions are raised as exceptions. This is similar to HUnit.
-/
abbrev Assertion := IO Unit

/--
  Fixtures are run in the IO monad and require a state of type σ and a value of type α.
  The state type can be set to Unit for fixtures without teardown code or without a state.

  The initial value for the state can be provided by defining `fixtureName.default` of type σ.
  This can be omitted if σ is an instance of `Inhabited`.

  See: https://leanprover.github.io/lean4/doc/monads/states.lean.html

  TODO: More documentation and relation to pytest.
-/
abbrev Fixture (σ : Type) (α : Type) := StateT σ IO α

/--
  The state type of the Teardown monad must match the state type of the fixture.
  Teardown functions do not return a value and must be named `fixtureName.teardown`.
-/
-- TODO: Remove the α parameter with the default value. This is only a workaround
--       for a possible bug during typeclass resolution.
abbrev Teardown (σ : Type) (α : Type := Unit) := StateT σ IO α
--abbrev Teardown (σ : Type) := StateT σ IO Unit

set_option linter.unusedVariables false in
/--
  Type used for parametrized testcases.

  This is the same as stating the type of the `values` argument directly.
  Example:
  ```lean
  @[testcase]
  def foo (a : param [1, 2, 3]) : Assertion := do return
  ```
  This defines a test `foo` where the argument `a` has type `Nat` and is parametrized.
-/
abbrev param {α : Type} (values : List α) := α

end LTest
