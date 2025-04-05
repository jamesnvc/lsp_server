#!/usr/bin/env bash

set -euo pipefail

declare -a load_test_files
for f in test/*.plt; do
    load_test_files+=( "-l" )
    load_test_files+=( "${f}" )
done
exec swipl --quiet ${load_test_files[@]} -g plunit:run_tests -g nl -t halt
