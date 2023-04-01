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

import LTest.Types
import Lean
open Lean

set_option relaxedAutoImplicit false

namespace LTest

/-- Convert a `ByteArray` into a list of numbers. -/
instance : ToJson ByteArray where
  toJson a := toJson $ a.toList.map fun n => n.toNat

/- Convert streams into a dictionary. -/
deriving instance ToJson for Streams

/-- Convert errors into their string representation. -/
instance : ToJson IO.Error where
  toJson e := toJson e.toString

/- Convert the okay branch of the capture result. -/
deriving instance ToJson for CaptureOkay

/- Convert the error branch of the capture result. -/
deriving instance ToJson for CaptureError

/-- Convert the Except type by adding a flag to indicate the branch. -/
instance [ToJson α] [ToJson β] : ToJson (Except α β) where
  toJson e := match e with
    | .ok v    => Json.mkObj [("ok", true),  ("value", toJson v)]
    | .error v => Json.mkObj [("ok", false), ("value", toJson v)]

/- Convert the test result into a dictionary. -/
deriving instance ToJson for TestResult

end LTest
