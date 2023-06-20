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

"""Test the correct execution order of fixtures and testcases."""

import pytest


def test_simple(program):
    """A simple trace where nothing fails."""
    CODE = """
        fixture A Unit Unit where
          default  := ()
          setup    := do trace "A.setup"
          teardown := do trace "A.teardown"

        fixture B Unit Unit where
          default  := ()
          setup    := do trace "B.setup"
          teardown := do trace "B.teardown"

        fixture C Unit Unit where
          default  := ()
          setup    := do trace "C.setup"
          teardown := do trace "C.teardown"

        testcase Foo requires (a : A) (b : B) (c : C) := do
          trace "testcase"
    """
    TRACE = [
        "A.setup",
        "B.setup",
        "C.setup",
        "testcase",
        "C.teardown",
        "B.teardown",
        "A.teardown",
    ]

    lake, lean = program(CODE)
    assert lean.returncode == 0
    assert lean.trace == TRACE


def test_simple_test_fail(program):
    """A simple trace where the testcase fails."""
    CODE = """
        fixture A Unit Unit where
          default  := ()
          setup    := do trace "A.setup"
          teardown := do trace "A.teardown"

        fixture B Unit Unit where
          default  := ()
          setup    := do trace "B.setup"
          teardown := do trace "B.teardown"

        fixture C Unit Unit where
          default  := ()
          setup    := do trace "C.setup"
          teardown := do trace "C.teardown"

        testcase Foo requires (a : A) (b : B) (c : C) := do
          trace "testcase"
          throw $ IO.Error.userError "Foo.error"
    """
    TRACE = [
        "A.setup",
        "B.setup",
        "C.setup",
        "testcase",
        "C.teardown",
        "B.teardown",
        "A.teardown",
    ]

    lake, lean = program(CODE)
    assert lean.returncode == 1
    assert lean.trace == TRACE


def test_simple_setup_fail(program):
    """A simple trace where a setup fails."""
    CODE = """
        fixture A Unit Unit where
          default  := ()
          setup    := do trace "A.setup"
          teardown := do trace "A.teardown"

        fixture B Unit Unit where
          default  := ()
          setup    := do
            trace "B.setup"
            throw $ IO.userError "B.error"
          teardown := do trace "B.teardown"

        fixture C Unit Unit where
          default  := ()
          setup    := do trace "C.setup"
          teardown := do trace "C.teardown"

        testcase Foo requires (a : A) (b : B) (c : C) := do trace "testcase"
    """
    TRACE = ["A.setup", "B.setup", "A.teardown"]

    lake, lean = program(CODE)
    assert lean.returncode == 1
    assert lean.trace == TRACE


def test_simple_teardown_fail(program):
    """A simple trace where a teardown fails."""
    CODE = """
        fixture A Unit Unit where
          default  := ()
          setup    := do trace "A.setup"
          teardown := do trace "A.teardown"

        fixture B Unit Unit where
          default  := ()
          setup    := do trace "B.setup"
          teardown := do
            trace "B.teardown"
            throw $ IO.userError "B.error"

        fixture C Unit Unit where
          default  := ()
          setup    := do trace "C.setup"
          teardown := do trace "C.teardown"

        testcase Foo requires (a : A) (b : B) (c : C) := do trace "testcase"
    """
    TRACE = [
        "A.setup",
        "B.setup",
        "C.setup",
        "testcase",
        "C.teardown",
        "B.teardown",
        "A.teardown",
    ]

    lake, lean = program(CODE)
    assert lean.returncode == 1
    assert lean.trace == TRACE


def test_nested(program):
    """A nested trace where nothing fails."""
    CODE = """
        fixture A1 Unit Unit where
          setup    := do trace "A1.setup"
          teardown := do trace "A1.teardown"

        fixture B1 Unit Unit where
          setup    := do trace "B1.setup"
          teardown := do trace "B1.teardown"

        fixture C1 Unit Unit requires (a : A1) (b : B1) where
          setup    := do trace "C1.setup"
          teardown := do trace "C1.teardown"

        fixture D1 Unit Unit where
          setup    := do trace "D1.setup"
          teardown := do trace "D1.teardown"

        fixture E1 Unit Unit where
          setup    := do trace "E1.setup"
          teardown := do trace "E1.teardown"

        fixture F1 Unit Unit requires (d : D1) (e : E1) where
          setup    := do trace "F1.setup"
          teardown := do trace "F1.teardown"

        fixture G1 Unit Unit requires (c : C1) (f : F1) where
          setup    := do trace "G1.setup"
          teardown := do trace "G1.teardown"

        fixture A2 Unit Unit where
          setup    := do trace "A2.setup"
          teardown := do trace "A2.teardown"

        fixture B2 Unit Unit where
          setup    := do trace "B2.setup"
          teardown := do trace "B2.teardown"

        fixture C2 Unit Unit requires (a : A2) (b : B2) where
          setup    := do trace "C2.setup"
          teardown := do trace "C2.teardown"

        fixture D2 Unit Unit where
          setup    := do trace "D2.setup"
          teardown := do trace "D2.teardown"

        fixture E2 Unit Unit where
          setup    := do trace "E2.setup"
          teardown := do trace "E2.teardown"

        fixture F2 Unit Unit requires (d : D2) (e : E2) where
          setup    := do trace "F2.setup"
          teardown := do trace "F2.teardown"

        fixture G2 Unit Unit requires (c : C2) (f : F2) where
          setup    := do trace "G2.setup"
          teardown := do trace "G2.teardown"

        testcase Foo requires (g1 : G1) (g2 : G2) := do trace "testcase"
    """
    TRACE = [
        "A1.setup",
        "B1.setup",
        "C1.setup",
        "D1.setup",
        "E1.setup",
        "F1.setup",
        "G1.setup",
        "A2.setup",
        "B2.setup",
        "C2.setup",
        "D2.setup",
        "E2.setup",
        "F2.setup",
        "G2.setup",
        "testcase",
        "G2.teardown",
        "F2.teardown",
        "E2.teardown",
        "D2.teardown",
        "C2.teardown",
        "B2.teardown",
        "A2.teardown",
        "G1.teardown",
        "F1.teardown",
        "E1.teardown",
        "D1.teardown",
        "C1.teardown",
        "B1.teardown",
        "A1.teardown",
    ]
    lake, lean = program(CODE)
    assert lean.returncode == 0
    assert lean.trace == TRACE


def test_nested_test_fail(program):
    """A nested trace where the testcase fails."""
    CODE = """
        fixture A1 Unit Unit where
          setup    := do trace "A1.setup"
          teardown := do trace "A1.teardown"

        fixture B1 Unit Unit where
          setup    := do trace "B1.setup"
          teardown := do trace "B1.teardown"

        fixture C1 Unit Unit requires (a : A1) (b : B1) where
          setup    := do trace "C1.setup"
          teardown := do trace "C1.teardown"

        fixture D1 Unit Unit where
          setup    := do trace "D1.setup"
          teardown := do trace "D1.teardown"

        fixture E1 Unit Unit where
          setup    := do trace "E1.setup"
          teardown := do trace "E1.teardown"

        fixture F1 Unit Unit requires (d : D1) (e : E1) where
          setup    := do trace "F1.setup"
          teardown := do trace "F1.teardown"

        fixture G1 Unit Unit requires (c : C1) (f : F1) where
          setup    := do trace "G1.setup"
          teardown := do trace "G1.teardown"

        fixture A2 Unit Unit where
          setup    := do trace "A2.setup"
          teardown := do trace "A2.teardown"

        fixture B2 Unit Unit where
          setup    := do trace "B2.setup"
          teardown := do trace "B2.teardown"

        fixture C2 Unit Unit requires (a : A2) (b : B2) where
          setup    := do trace "C2.setup"
          teardown := do trace "C2.teardown"

        fixture D2 Unit Unit where
          setup    := do trace "D2.setup"
          teardown := do trace "D2.teardown"

        fixture E2 Unit Unit where
          setup    := do trace "E2.setup"
          teardown := do trace "E2.teardown"

        fixture F2 Unit Unit requires (d : D2) (e : E2) where
          setup    := do trace "F2.setup"
          teardown := do trace "F2.teardown"

        fixture G2 Unit Unit requires (c : C2) (f : F2) where
          setup    := do trace "G2.setup"
          teardown := do trace "G2.teardown"

        testcase Foo requires (g1 : G1) (g2 : G2) := do
          trace "testcase"
          throw $ IO.Error.userError "Foo.error"
    """
    TRACE = [
        "A1.setup",
        "B1.setup",
        "C1.setup",
        "D1.setup",
        "E1.setup",
        "F1.setup",
        "G1.setup",
        "A2.setup",
        "B2.setup",
        "C2.setup",
        "D2.setup",
        "E2.setup",
        "F2.setup",
        "G2.setup",
        "testcase",
        "G2.teardown",
        "F2.teardown",
        "E2.teardown",
        "D2.teardown",
        "C2.teardown",
        "B2.teardown",
        "A2.teardown",
        "G1.teardown",
        "F1.teardown",
        "E1.teardown",
        "D1.teardown",
        "C1.teardown",
        "B1.teardown",
        "A1.teardown",
    ]
    lake, lean = program(CODE)
    assert lean.returncode == 1
    assert lean.trace == TRACE


def test_nested_setup_fail(program):
    """A nested trace where a setup fails."""
    CODE = """
        fixture A1 Unit Unit where
          setup    := do trace "A1.setup"
          teardown := do trace "A1.teardown"

        fixture B1 Unit Unit where
          setup    := do trace "B1.setup"
          teardown := do trace "B1.teardown"

        fixture C1 Unit Unit requires (a : A1) (b : B1) where
          setup    := do trace "C1.setup"
          teardown := do trace "C1.teardown"

        fixture D1 Unit Unit where
          setup    := do trace "D1.setup"
          teardown := do trace "D1.teardown"

        fixture E1 Unit Unit where
          setup    := do trace "E1.setup"
          teardown := do trace "E1.teardown"

        fixture F1 Unit Unit requires (d : D1) (e : E1) where
          setup    := do trace "F1.setup"
          teardown := do trace "F1.teardown"

        fixture G1 Unit Unit requires (c : C1) (f : F1) where
          setup    := do trace "G1.setup"
          teardown := do trace "G1.teardown"

        fixture A2 Unit Unit where
          setup    := do trace "A2.setup"
          teardown := do trace "A2.teardown"

        fixture B2 Unit Unit where
          setup    := do trace "B2.setup"
          teardown := do trace "B2.teardown"

        fixture C2 Unit Unit requires (a : A2) (b : B2) where
          setup    := do trace "C2.setup"
          teardown := do trace "C2.teardown"

        fixture D2 Unit Unit where
          setup    := do
            trace "D2.setup"
            throw $ IO.userError "D2.error"
          teardown := do trace "D2.teardown"

        fixture E2 Unit Unit where
          setup    := do trace "E2.setup"
          teardown := do trace "E2.teardown"

        fixture F2 Unit Unit requires (d : D2) (e : E2) where
          setup    := do trace "F2.setup"
          teardown := do trace "F2.teardown"

        fixture G2 Unit Unit requires (c : C2) (f : F2) where
          setup    := do trace "G2.setup"
          teardown := do trace "G2.teardown"

        testcase Foo requires (g1 : G1) (g2 : G2) := do trace "testcase"
    """
    TRACE = [
        "A1.setup",
        "B1.setup",
        "C1.setup",
        "D1.setup",
        "E1.setup",
        "F1.setup",
        "G1.setup",
        "A2.setup",
        "B2.setup",
        "C2.setup",
        "D2.setup",
        "C2.teardown",
        "B2.teardown",
        "A2.teardown",
        "G1.teardown",
        "F1.teardown",
        "E1.teardown",
        "D1.teardown",
        "C1.teardown",
        "B1.teardown",
        "A1.teardown",
    ]
    lake, lean = program(CODE)
    assert lean.returncode == 1
    assert lean.trace == TRACE


def test_nested_teardown_fail(program):
    """A nested trace where a teardown fails."""
    CODE = """
        fixture A1 Unit Unit where
          setup    := do trace "A1.setup"
          teardown := do trace "A1.teardown"

        fixture B1 Unit Unit where
          setup    := do trace "B1.setup"
          teardown := do trace "B1.teardown"

        fixture C1 Unit Unit requires (a : A1) (b : B1) where
          setup    := do trace "C1.setup"
          teardown := do trace "C1.teardown"

        fixture D1 Unit Unit where
          setup    := do trace "D1.setup"
          teardown := do trace "D1.teardown"

        fixture E1 Unit Unit where
          setup    := do trace "E1.setup"
          teardown := do trace "E1.teardown"

        fixture F1 Unit Unit requires (d : D1) (e : E1) where
          setup    := do trace "F1.setup"
          teardown := do trace "F1.teardown"

        fixture G1 Unit Unit requires (c : C1) (f : F1) where
          setup    := do trace "G1.setup"
          teardown := do trace "G1.teardown"

        fixture A2 Unit Unit where
          setup    := do trace "A2.setup"
          teardown := do trace "A2.teardown"

        fixture B2 Unit Unit where
          setup    := do trace "B2.setup"
          teardown := do trace "B2.teardown"

        fixture C2 Unit Unit requires (a : A2) (b : B2) where
          setup    := do trace "C2.setup"
          teardown := do trace "C2.teardown"

        fixture D2 Unit Unit where
          setup    := do trace "D2.setup"
          teardown := do
            trace "D2.teardown"
            throw $ IO.userError "D2.error"

        fixture E2 Unit Unit where
          setup    := do trace "E2.setup"
          teardown := do trace "E2.teardown"

        fixture F2 Unit Unit requires (d : D2) (e : E2) where
          setup    := do trace "F2.setup"
          teardown := do trace "F2.teardown"

        fixture G2 Unit Unit requires (c : C2) (f : F2) where
          setup    := do trace "G2.setup"
          teardown := do trace "G2.teardown"

        testcase Foo requires (g1 : G1) (g2 : G2) := do trace "testcase"
    """
    TRACE = [
        "A1.setup",
        "B1.setup",
        "C1.setup",
        "D1.setup",
        "E1.setup",
        "F1.setup",
        "G1.setup",
        "A2.setup",
        "B2.setup",
        "C2.setup",
        "D2.setup",
        "E2.setup",
        "F2.setup",
        "G2.setup",
        "testcase",
        "G2.teardown",
        "F2.teardown",
        "E2.teardown",
        "D2.teardown",
        "C2.teardown",
        "B2.teardown",
        "A2.teardown",
        "G1.teardown",
        "F1.teardown",
        "E1.teardown",
        "D1.teardown",
        "C1.teardown",
        "B1.teardown",
        "A1.teardown",
    ]
    lake, lean = program(CODE)
    assert lean.returncode == 1
    assert lean.trace == TRACE


def test_anonymous_dependency_testcase(program):
    """A simple trace with anonymous names."""
    CODE = """
        fixture A Unit Unit where
          default  := ()
          setup    := do trace "A.setup"
          teardown := do trace "A.teardown"

        fixture B Unit Unit where
          default  := ()
          setup    := do trace "B.setup"
          teardown := do trace "B.teardown"

        fixture C Unit Unit where
          default  := ()
          setup    := do trace "C.setup"
          teardown := do trace "C.teardown"

        testcase Foo requires (_ : A) (b : B) (_ : C) := do
          trace "testcase"
    """
    TRACE = [
        "A.setup",
        "B.setup",
        "C.setup",
        "testcase",
        "C.teardown",
        "B.teardown",
        "A.teardown",
    ]

    lake, lean = program(CODE)
    assert lean.returncode == 0
    assert lean.trace == TRACE


def test_anonymous_dependency_fixture(program):
    """A simple trace with anonymous names."""
    CODE = """
        fixture A Unit Unit where
          default  := ()
          setup    := do trace "A.setup"
          teardown := do trace "A.teardown"

        fixture B Unit Unit where
          default  := ()
          setup    := do trace "B.setup"
          teardown := do trace "B.teardown"

        fixture C Unit Unit requires (_ : A) (_ : B) where
          default  := ()
          setup    := do trace "C.setup"
          teardown := do trace "C.teardown"

        testcase Foo requires (_ : C) := do
          trace "testcase"
    """
    TRACE = [
        "A.setup",
        "B.setup",
        "C.setup",
        "testcase",
        "C.teardown",
        "B.teardown",
        "A.teardown",
    ]

    lake, lean = program(CODE)
    assert lean.returncode == 0
    assert lean.trace == TRACE
