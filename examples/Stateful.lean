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

/-- State of the `Counter` fixture. -/
initialize counterValue : IO.Ref Nat ← IO.mkRef 0

/--
  We use the `counterValue` reference as the state of the fixture to maintain a state
  across multiple fixture calls in the same testcase.

  We couldn't implement this with a `Nat` state alone because every fixture instance
  gets a new state.
-/
fixture StatefulCounter Unit Nat where
  setup    := counterValue.modifyGet fun n => (n, n + 1)
  teardown := counterValue.modify    fun n => n - 1

/--
  For comparison with a `Nat` state.
-/
fixture StatelessCounter Nat Nat where
  setup    := modifyGet fun n => (n, n + 1)
  teardown := modify    fun n => n - 1


/-- The `StatefulCounter` fixture has a persistent state across instances. -/
testcase testStatefulCounter requires (a : StatefulCounter) (b : StatefulCounter) := do
  assertTrue (a == 0)
  assertTrue (b == 1)

/-- The `StatelessCounter` fixture doesn't. -/
testcase testStatelessCounter requires (a : StatelessCounter) (b : StatelessCounter) := do
  assertTrue (a == 0)
  assertTrue (b == 0)


/-! Generate the main function. -/
#LTestMain
