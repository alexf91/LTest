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

set_option relaxedAutoImplicit false


namespace LTest

/-- Assert that the proposition `p` is true. -/
def assertTrue (p : Bool) (msg : String := "assertion failed") : IO Unit := do
  unless p do
    throw $ IO.userError msg

/-- Assert that the proposition `p` is false. -/
def assertFalse (p : Bool) (msg : String := "assertion failed") : IO Unit := do
  unless !p do
    throw $ IO.userError msg

/-- Assert that `a` is equal to `b`. -/
def assertEqual [BEq α] (a : α) (b : α) (msg : String := "assertion failed") : IO Unit := do
  unless a == b do
    throw $ IO.userError msg

end LTest
