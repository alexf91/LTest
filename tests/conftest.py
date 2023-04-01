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

import json
import os
import shutil
import subprocess as sp
import textwrap

import pytest


TESTDIR = os.path.dirname(__file__)

LAKEFILE = """
    import Lake
    open Lake DSL

    package Tests {{
        packagesDir := "{packagesdir}"
    }}

    lean_lib Library
    require LTest from "{testdir}/.."
    require TestUtils from "{testdir}/utils"

    @[default_target]
    lean_exe Program
"""

CODE_TEMPLATE = """
    import LTest
    import TestUtils
    open LTest
    open TestUtils

    {CODE}

    #LTestMain
"""


def render_code(code):
    """Add the code to the program template."""
    template = textwrap.dedent(CODE_TEMPLATE)
    code = textwrap.dedent(code)
    return template.format(CODE=code)


def load_results(fp):
    """Load the JSON result and convert a few fields.

    * Turn it into a dictionary
    * TODO: Convert stdout and stderr to byte arrays
    """
    result = dict(json.load(fp))
    return result


@pytest.fixture(scope="session")
def lakefile(pytestconfig):
    """Download Lean packages to speed up building and return the Lakefile."""
    path = pytestconfig.cache.mkdir("lake-packages")
    content = LAKEFILE.format(testdir=TESTDIR, packagesdir=path)
    return textwrap.dedent(content)


@pytest.fixture
def program(tmp_path, lakefile):
    """Create a lean program and compile it."""

    def factory(code, check_compile=True, raw=False):
        # Copy the toolchain file so we use the correct one.
        shutil.copy(os.path.join(TESTDIR, "..", "lean-toolchain"), tmp_path)

        # Write lakefile and program.
        with open(tmp_path / "lakefile.lean", "w") as fp:
            fp.write(lakefile)
        with open(tmp_path / "Program.lean", "w") as fp:
            if raw:
                fp.write(textwrap.dedent(code))
            else:
                fp.write(render_code(code))

        # Build the program.
        lake = sp.run(
            ["lake", "build", "-v", "-U"],
            capture_output=True,
            check=check_compile,
            encoding="utf8",
            cwd=tmp_path,
        )

        # Run the program with JSON output.
        lean = sp.run(
            ["build/bin/Program", "--json-output", "results.json"],
            capture_output=True,
            encoding="utf8",
            cwd=tmp_path,
        )

        # Open the JSON file and add it to the lean result.
        try:
            with open(tmp_path / "results.json") as fp:
                lean.results = load_results(fp)
        except FileNotFoundError:
            lean.results = None

        # Open the trace file and add it to the lean result.
        try:
            with open(tmp_path / "results.trace") as fp:
                lean.trace = [line.rstrip() for line in fp.readlines()]
        except FileNotFoundError:
            lean.trace = None

        return (lake, lean)

    yield factory
