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
  Run a testcase with fixtures where nothing fails.
-/

import TestUtils
import LTest
open TestUtils
open LTest


fixture A1 Unit Unit where
  setup    := do trace "A1.setup"
  teardown := do trace "A1.teardown"

fixture B1 Unit Unit requires (a : A1) where
  setup    := do trace "B1.setup"
  teardown := do trace "B1.teardown"

fixture C1 Unit Unit requires (b : B1) where
  setup    := do trace "C1.setup"
  teardown := do trace "C1.teardown"

fixture A2 Unit Unit where
  setup    := do trace "A2.setup"
  teardown := do trace "A2.teardown"

fixture B2 Unit Unit requires (a : A2) where
  setup    := do trace "B2.setup"
  teardown := do trace "B2.teardown"

fixture C2 Unit Unit requires (b : B2) where
  setup    := do trace "C2.setup"
  teardown := do trace "C2.teardown"

fixture A3 Unit Unit where
  setup    := do trace "A3.setup"
  teardown := do trace "A3.teardown"

fixture B3 Unit Unit requires (a : A3) where
  setup    := do trace "B3.setup"
  teardown := do trace "B3.teardown"

fixture C3 Unit Unit requires (b : B3) where
  setup    := do trace "C3.setup"
  teardown := do trace "C3.teardown"

testcase Foo requires (c1 : C1) (c2 : C2) (c3 : C3) := do trace "testcase"

#LTestMain
