{
  pkgs,
  gennifer,
  gcc,
}:

pkgs.writeShellApplication {
  name = "gennifer-gcc";

  runtimeInputs = [
    gcc
    gennifer
  ];

  text = ''
    set -euo pipefail

    if [ $# -lt 1 ] || [ $# -gt 2 ]; then
      echo "usage: gennifer-gcc <source.gir> [-O0|-O1|-O2|-O3]"
      exit 1
    fi

    FILE="$1"
    OPT="-O2"   # default

    if [ $# -eq 2 ]; then
      case "$2" in
        -O0|-O1|-O2|-O3)
          OPT="$2"
          ;;
        *)
          echo "error: invalid optimization flag '$2'"
          echo "allowed: -O0 -O1 -O2 -O3"
          exit 1
          ;;
      esac
    fi

    gennifer "$FILE" --c -o "$FILE.c" --pt

    time gcc "$OPT" "$FILE.c" -o "$FILE.c.out" \
      -lm \
      -l:libmemoization.a \
      -l:libmurmur_hash3.a
  '';
}