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

/-
  One setup fails.

  Teardown for successfully executed setup functions should be executed.
-/

import TestUtils
import LTest
open TestUtils
open LTest


fixture A Unit Unit where
  default  := ()
  setup    := do trace "A.setup"
  teardown := do trace "A.teardown"

fixture B Unit Unit where
  default  := ()
  setup    := do
    trace "B.setup"
    throw $ IO.userError "B.error"
  teardown := do trace "B.teardown"

fixture C Unit Unit where
  default  := ()
  setup    := do trace "C.setup"
  teardown := do trace "C.teardown"

testcase Foo requires (a : A) (b : B) (c : C) := do trace "testcase"

#LTestMain
