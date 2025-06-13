{ lib, pkgs }:

pkgs.writeShellApplication {
  name = "profiling";
  runtimeInputs = [ pkgs.linuxKernel.packages.linux_zen.perf ]; # This ensures pkgs.perf is included as a runtime dependency
  text = ''
    #!/usr/bin/env bash

    echo "USAGE: ./profiling.sh <binary> <args>"

    set -euo pipefail
    IFS=$'\n\t'

    echo "RUNNING perf ON $*"
    perf record --call-graph=dwarf "$@"
    echo "DONE. CONVERTING TO TEXT FILE profile.perf..."
    (perf script -F +pid) > profile.perf
    echo "DONE. You can view profile.perf on https://profiler.firefox.com/"
  '';
}