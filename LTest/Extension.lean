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

import LTest.Runtime
import Lean
open Lean
open Lean.Elab
open Lean.Elab.Command


set_option relaxedAutoImplicit false

namespace LTest


/-
  Environment extension for discovery.
-/
initialize testcaseExtension : SimplePersistentEnvExtension Name (List Name) ←
  registerSimplePersistentEnvExtension {
    addEntryFn    := List.concat
    addImportedFn := mkStateFromImportedEntries List.concat {}
  }

scoped syntax (name := insertTestcase) "_ltest_insert_testcase " ident : command
scoped syntax (name := showTestcases) "_ltest_show_testcases" : command

/--
  Register a testcase with the extension.
-/
@[command_elab insertTestcase]
private def elabInsertTestcase : CommandElab := fun stx => do
  let name ← resolveGlobalConstNoOverload stx[1]
  modifyEnv fun env => testcaseExtension.addEntry env name


/--
  Get testcases registered with the extension.
-/
def getTestcases [Monad m] [MonadEnv m] : m (Array Name) := do
  return testcaseExtension.getState (← getEnv) |>.toArray


end LTest
