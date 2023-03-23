--
-- Copyright 2023 Alexander Fasching
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.
--

import Lake
open Lake DSL

package LTest {
  precompileModules := true
}

@[default_target]
lean_lib LTest

-- Script to build and run tests.
script tests (args : List String) do
  let process ← IO.Process.spawn {cmd := "tests/run", args := args.toArray}
  let result ← process.wait
  return result
