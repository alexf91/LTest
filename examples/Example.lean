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
  default := ()
  setup := do return 0


/--
  Define a testcase with fixture dependencies.

  The body of the testcase can access `a` and `b` of type `Nat`.
-/
testcase testZero requires (a : Zero) (b : Zero) := do
  IO.println s!"a: {a}"
  IO.println s!"b: {b}"
  return


/-- Fixtures can also have dependencies. -/
fixture One Unit Nat requires (n : Zero) where
  default := ()
  setup := do return n + 1


testcase testOne requires (a : One) :=
  IO.println s!"a: {a}"


/-! Generate the main function. -/
#LTestMain
