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

import LTest.Instances
import Lean
open Lean (Name)

set_option relaxedAutoImplicit false

namespace LTest.Report

/-- Get the number of columns in the terminal. -/
def columns : IO Nat := do
  try
    let out ← IO.Process.run {cmd := "tput", args := #["cols"]}
    match out.trim.toNat? with
    | some n => return n
    | _      => return 80
  catch _ =>
    return 80

/-- Center the string `s` in the terminal and pad it with `c`. -/
def center (s : String) (c : Char := ' ') : IO String := do
  let cols ← columns
  let lp := (cols - s.length) / 2
  let rp := (cols - s.length - lp)
  return (c.toString * lp) ++ s ++ (c.toString * rp)

end LTest.Report
