#!/usr/bin/env bash

range=1000000000

here=$(dirname "$0")
EXEC="bench_hashset --lib hashbrown"
file_name="bench_hashset"
log_name="${EXEC// /}"
folder_name=$(date +"%Y-%m-%d_%H-%M-%S")
prefix="${here}/${folder_name}/${file_name}"

mkdir "${here}/${folder_name}"

{
hyperfine --export-csv "${prefix}-${log_name}.csv" \
          --show-output \
          --runs 1 \
          --parameter-scan length 1 "${range}" -D 1000000 \
          "${EXEC} {length}" 
} 2>&1 | tee "${prefix}-${log_name}.log"