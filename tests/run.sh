#!/bin/sh
#
# Copyright 2023 Alexander Fasching
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program. If not, see <http://www.gnu.org/licenses/>.
#

if [ $# != 0 ]; then
  echo "Usage: $0"
  exit 1
fi

lake update
lake clean

# Run all tests in a temporary directory.
WORKDIR=$(mktemp -d --tmpdir LTest.XXXXXX)
echo "Running in $WORKDIR"

EXITCODE=0

# Build and run every test_*.lean file.
for file in $(ls test_*.lean); do
  TARGET=$(basename -s .lean $file)
  lake build $TARGET

  mkdir "$WORKDIR/$TARGET"
  TEST="$PWD/build/bin/$TARGET"
  (cd "$WORKDIR/$TARGET" && "$TEST" > stdout)

  # Check outputs and tracefile.
  if [ -e "$TARGET.expected.trace" ]; then
    if ! diff "$WORKDIR/$TARGET/trace" "$TARGET.expected.trace"; then
      echo "[FAIL] trace mismatch for $TARGET"
      EXITCODE=1
    fi
  fi

  if [ -e "$TARGET.expected.stdout" ]; then
    if ! diff "$WORKDIR/$TARGET/stdout" "$TARGET.expected.stdout"; then
      echo "[FAIL] stdout mismatch for $TARGET"
      EXITCODE=1
    fi
  fi

  if [ -e "$TARGET.expected.stderr" ]; then
    if ! diff "$WORKDIR/$TARGET/stderr" "$TARGET.expected.stderr"; then
      echo "[FAIL] stderr mismatch for $TARGET"
      EXITCODE=1
    fi
  fi

done

exit $EXITCODE
