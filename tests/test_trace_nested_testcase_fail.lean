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

fixture B1 Unit Unit where
  setup    := do trace "B1.setup"
  teardown := do trace "B1.teardown"

fixture C1 Unit Unit requires (a : A1) (b : B1) where
  setup    := do trace "C1.setup"
  teardown := do trace "C1.teardown"

fixture D1 Unit Unit where
  setup    := do trace "D1.setup"
  teardown := do trace "D1.teardown"

fixture E1 Unit Unit where
  setup    := do trace "E1.setup"
  teardown := do trace "E1.teardown"

fixture F1 Unit Unit requires (d : D1) (e : E1) where
  setup    := do trace "F1.setup"
  teardown := do trace "F1.teardown"

fixture G1 Unit Unit requires (c : C1) (f : F1) where
  setup    := do trace "G1.setup"
  teardown := do trace "G1.teardown"

fixture A2 Unit Unit where
  setup    := do trace "A2.setup"
  teardown := do trace "A2.teardown"

fixture B2 Unit Unit where
  setup    := do trace "B2.setup"
  teardown := do trace "B2.teardown"

fixture C2 Unit Unit requires (a : A2) (b : B2) where
  setup    := do trace "C2.setup"
  teardown := do trace "C2.teardown"

fixture D2 Unit Unit where
  setup    := do trace "D2.setup"
  teardown := do trace "D2.teardown"

fixture E2 Unit Unit where
  setup    := do trace "E2.setup"
  teardown := do trace "E2.teardown"

fixture F2 Unit Unit requires (d : D2) (e : E2) where
  setup    := do trace "F2.setup"
  teardown := do trace "F2.teardown"

fixture G2 Unit Unit requires (c : C2) (f : F2) where
  setup    := do trace "G2.setup"
  teardown := do trace "G2.teardown"

testcase Foo requires (g1 : G1) (g2 : G2) := do
  trace "testcase"
  throw $ IO.userError "testcase.error"

#LTestMain
