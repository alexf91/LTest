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


fixture A Unit Unit where
  setup    := do trace "A.setup"
  teardown := do trace "A.teardown"

fixture B Unit Unit where
  setup    := do trace "B.setup"
  teardown := do trace "B.teardown"

fixture C Unit Unit requires (a : A) (b : B) where
  setup    := do trace "C.setup"
  teardown := do trace "C.teardown"

fixture D Unit Unit where
  setup    := do trace "D.setup"
  teardown := do trace "D.teardown"

fixture E Unit Unit where
  setup    := do trace "E.setup"
  teardown := do trace "E.teardown"

fixture F Unit Unit requires (d : D) (e : E) where
  setup    := do trace "F.setup"
  teardown := do trace "F.teardown"

fixture G Unit Unit requires (c : C) (f : F) where
  setup    := do trace "G.setup"
  teardown := do trace "G.teardown"

testcase Foo requires (g : G) := do trace "testcase"

#LTestMain
