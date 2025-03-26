#!/usr/bin/env bash

exec swipl -l test/formatter.plt -g plunit:run_tests -t halt
