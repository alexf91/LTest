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

import LTest
open LTest


/-- Define a simple testcase without requirements. -/
testcase testEmpty := do return


/-- Define a fixture without a state that always returns 0. -/
fixture Zero Unit Nat where
  default  := ()
  setup    := do return 0
  teardown := do return


/--
  Define a testcase with fixture dependencies.

  The body of the testcase can access `a` and `b` of type `Nat`.
-/
testcase testZero requires (a : Zero) (b : Zero) := do
  IO.println s!"a: {a}"
  IO.println s!"b: {b}"


/-- No tests without assertions. -/
testcase testSomething := do
  assertTrue (0 == 0)


/--
  Fixture with dependencies.

  If we omit `teardown`, then it's set to `do return` by default.
  The `default` state can be omitted if `Inhabited σ` can be synthesized.
-/
fixture One Unit Nat requires (n : Zero) where
  setup := do
    return n + 1


testcase testOne requires (n : One) := do
  assertTrue (n == 1)


/-
  Tests can be defined in namespaces for easier selection and grouping.
  Before execution, tests are sorted by their fully qualified name.
-/
namespace Foo
  testcase testHam := do
    return

  testcase testEggs := do
    return
end Foo


/-! Generate the main function. -/
#LTestMain
