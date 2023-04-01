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

"""Test capturing of test results and output streams."""

import pytest


def test_simple_result_success(program):
    """Capture the output of a standalone test without fixtures."""
    CODE = """
        testcase Foo := do
          IO.println  "Foo.stdout"
          IO.eprintln "Foo.stderr"
    """
    lake, lean = program(CODE)
    assert lean.returncode == 0

    r = lean.results["Foo"]
    assert r["setupResults"] == []
    assert r["teardownResults"] == []
    assert r["testcaseResult"]["ok"]
    assert bytes(r["testcaseResult"]["value"]["stdout"]) == b"Foo.stdout\n"
    assert bytes(r["testcaseResult"]["value"]["stderr"]) == b"Foo.stderr\n"


def test_simple_result_error(program):
    """Capture the output of a standalone test without fixtures."""
    CODE = """
        testcase Foo := do
          IO.println  "Foo.stdout"
          IO.eprintln "Foo.stderr"
          throw $ IO.Error.userError "Foo.error"
          IO.println  "Foo.stdout.unreachable"
          IO.eprintln "Foo.stderr.unreachable"
    """
    lake, lean = program(CODE)
    assert lean.returncode == 1

    r = lean.results["Foo"]
    assert r["setupResults"] == []
    assert r["teardownResults"] == []
    assert not r["testcaseResult"]["ok"]
    assert r["testcaseResult"]["value"][0] == "Foo.error"
    assert bytes(r["testcaseResult"]["value"][1]["stdout"]) == b"Foo.stdout\n"
    assert bytes(r["testcaseResult"]["value"][1]["stderr"]) == b"Foo.stderr\n"
