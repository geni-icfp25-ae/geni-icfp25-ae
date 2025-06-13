#!/usr/bin/env bash

echo "USAGE: ./profiling.sh <binary> <args>"

set -euo pipefail
IFS=$'\n\t'

echo "RUNNING perf ON $*"
sudo perf record --call-graph=dwarf "$@"
echo "DONE. CONVERTING TO TEXT FILE profile.perf..."
(sudo perf script -F +pid) > profile.perf
echo "DONE. You can view profile.perf on https://profiler.firefox.com/"