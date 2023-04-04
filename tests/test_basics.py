#
# Copyright 2023 Alexander Fasching
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

import pytest


def test_main(program):
    """Test main function creation."""
    CODE = """
        import LTest
        #LTestMain
    """
    lake, _ = program(CODE, raw=True, check_compile=False)
    lake.returncode == 0


def test_main_fail(program):
    """Test main function creation."""
    CODE = """#LTestMain"""
    lake, _ = program(CODE, raw=True, check_compile=False)
    lake.returncode == 1


def test_syntax_noopen(program):
    """Test if macros are available without opening LTest."""
    CODE = """
        import LTest
        testcase Foo := do return
        #LTestMain
    """
    lake, _ = program(CODE, raw=True, check_compile=False)
    lake.returncode == 0


def test_syntax_open(program):
    """Test if macros are available with opening LTest."""
    CODE = """
        import LTest
        open LTest
        testcase Foo := do return
        #LTestMain
    """
    lake, _ = program(CODE, raw=True, check_compile=False)
    lake.returncode == 0
