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

package Example {
  precompileModules := true
}

require LTest from ".."

@[default_target]
lean_exe Example

-- Script to build and run tests.
script tests (args : List String) do
  discard <| IO.Process.run {cmd := "lake", args := #["build", "Example"]}
  let process ← IO.Process.spawn {cmd := "build/bin/Example", args := args.toArray}
  let result ← process.wait
  return result
