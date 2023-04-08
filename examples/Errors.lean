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

/-- Fixture with an error during setup. -/
fixture FooSetupError Unit Unit where
  setup := do
    IO.println "output of FooSetupError"
    throw $ IO.userError "something happened during setup"

/-- Fixture with an error during teardown. -/
fixture FooTeardownError Unit Unit where
  setup := do return
  teardown := do
    IO.println "output of FooTeardownError"
    throw $ IO.userError "something happened during teardown"

testcase testFooSetup requires (a : FooSetupError) := do return
testcase testFooTeardown requires (a : FooTeardownError) := do return


/-- Errors in fixture dependencies are also reported. -/
fixture BarError Unit Unit requires (a : FooTeardownError) (b : FooSetupError) where
  setup := do return

testcase testBar requires (a : BarError) := do return

/-! Generate the main function. -/
#LTestMain
