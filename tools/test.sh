#!/usr/bin/env bash
# tools/test.sh --- Run the package's tests.
# Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
# Run this package's tests against a Tuatara source tree: $TUATARA_SOURCE,
# else tuatara beside this repository or beside the folder it is in. In
# Tuatara's toolbox when there is one, as Tuatara's own tests run.
# Usage: tools/test.sh
set -euo pipefail
cd "$(dirname "$0")/.."
if [ -z "${TUATARA_SOURCE:-}" ]; then
  for candidate in ../tuatara ../../tuatara; do
    if [ -d "$candidate" ]; then TUATARA_SOURCE="$candidate"; break; fi
  done
fi
[ -n "${TUATARA_SOURCE:-}" ] || { echo "No Tuatara source tree beside this repository: set TUATARA_SOURCE" >&2; exit 1; }
TUATARA_SOURCE="$(realpath "$TUATARA_SOURCE")"
export TUATARA_SOURCE
. "$TUATARA_SOURCE/tools/inside.sh"
inside env TUATARA_SOURCE="$TUATARA_SOURCE" sbcl --dynamic-space-size 4GB --non-interactive --load test/run.lisp
