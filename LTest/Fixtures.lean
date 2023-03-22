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

import LTest.DSL
import Lean
open Lean

set_option relaxedAutoImplicit false

namespace LTest

/--
  Create a temporary directory and delete it after the test is finished.
-/
fixture TempDir (Option System.FilePath) System.FilePath where
  default := none
  setup := do
    let output ← IO.Process.run {
      cmd := "mktemp"
      args := #["-d", "--tmpdir", "LTest.tempdir.XXXXXXXXXX"]
    }
    let path := System.FilePath.mk output.trim
    set $ some path
    return path
  teardown := do
    if let some path ← get then
      discard $ IO.Process.run {
        cmd := "rm"
        args := #["-rf", path.toString]
      }

end LTest
