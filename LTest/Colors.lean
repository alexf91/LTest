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

import Lean

set_option relaxedAutoImplicit false

namespace LTest


/-- Definitions for escape codes. -/
inductive Color where
  | black
  | red
  | green
  | yellow
  | blue
  | purple
  | cyan
  | white
  | bBlack
  | bRed
  | bGreen
  | bYellow
  | bBlue
  | bPurple
  | bCyan
  | bWhite
  | noColor


namespace Color
  /-- Start of the escape sequence (`\033`). -/
  private def ESC := String.fromUTF8Unchecked $ .mk #[0x1b]
  /-- Convert a color to a terminal escape sequence. -/
  def ansiEscape (c : Color) : String := match c with
    | .black   => ESC ++ "[0;30m"
    | .red     => ESC ++ "[0;31m"
    | .green   => ESC ++ "[0;32m"
    | .yellow  => ESC ++ "[0;33m"
    | .blue    => ESC ++ "[0;34m"
    | .purple  => ESC ++ "[0;35m"
    | .cyan    => ESC ++ "[0;36m"
    | .white   => ESC ++ "[0;37m"
    | .bBlack  => ESC ++ "[1;30m"
    | .bRed    => ESC ++ "[1;31m"
    | .bGreen  => ESC ++ "[1;32m"
    | .bYellow => ESC ++ "[1;33m"
    | .bBlue   => ESC ++ "[1;34m"
    | .bPurple => ESC ++ "[1;35m"
    | .bCyan   => ESC ++ "[1;36m"
    | .bWhite  => ESC ++ "[1;37m"
    | .noColor => ESC ++ "[0m"
end Color

end LTest
