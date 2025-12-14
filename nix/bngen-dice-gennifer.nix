{ pkgs, bngen, dice }:

pkgs.writeShellApplication {
  name = "bngen-dice-gennifer";

  runtimeInputs = [
    bngen   # replace with the correct package/derivation name if different
    dice    # replace with the correct package/derivation name if different
  ];

  text = ''
    set -euo pipefail

    if [ $# -ne 2 ]; then
      echo "usage: bngen-dice-gennifer <source.bif> <id>"
      exit 1
    fi

    file="$1"
    id="$2"

    bngen single "$file" "$id" > "$file.dice"
    dice "$file.dice" -show-internal-gennifer-nowrap -skip-table > "$file.dice.gir"
  '';
}
