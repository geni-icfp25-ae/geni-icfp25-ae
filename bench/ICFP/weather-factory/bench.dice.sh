#!/usr/bin/env bash

here=$(dirname "$0")
EXEC="dice"
folder_name=$(date +"%Y-%m-%d_%H-%M-%S")
path_to_folder="${here}/${folder_name}"
log_name="${EXEC// /}"

mkdir "${path_to_folder}"

# Set SANITY_CHECK to 1 to enable, or leave unset/0 to disable
SANITY_CHECK=${SANITY_CHECK:-0}

if [[ "${SANITY_CHECK}" -eq 1 ]]; then
    warmup_runs=0
    hyperf_runs=1
else
    warmup_runs=0
    hyperf_runs=1
fi

if [[ "$SANITY_CHECK" -eq 1 ]]; then
    h_list="1,2,3,4,5,6,7,8,9,10,100,200,300,400,500"
    n_list="2,7,8,9,10,11,12,13,15,16"
else
    h_list="1,2,3,4,5,6,7,8,9,10,100,200,300,400,500,600,700,800,900,1000"
    n_list="2,7,8,9,10,11,12,13,15,16,17"
fi

{
    hyperfine --export-csv "${path_to_folder}/${folder_name}.${log_name}.test_horizon.csv" \
        --show-output \
        --warmup ${warmup_runs} \
        --runs ${hyperf_runs} \
        --parameter-list h "${h_list}" \
        --parameter-list n "10" \
        "${EXEC} ${here}/rubicon-dice/weather-factory-{n}-{h}.dice"
} 2>&1 | tee "${path_to_folder}/${folder_name}.${log_name}.test_horizon.log"

cp "${path_to_folder}/${folder_name}.${log_name}.test_horizon.csv" "${here}/${log_name}.test_horizon.csv"

{
    hyperfine --export-csv "${path_to_folder}/${folder_name}.${log_name}.test_factory.csv" \
        --show-output \
        --warmup ${warmup_runs} \
        --runs ${hyperf_runs} \
        --parameter-list n "${n_list}" \
        --parameter-list h "10" \
        "${EXEC} ${here}/rubicon-dice/weather-factory-{n}-{h}.dice"
} 2>&1 | tee "${path_to_folder}/${folder_name}.${log_name}.test_factory.log"

cp "${path_to_folder}/${folder_name}.${log_name}.test_factory.csv" "${here}/${log_name}.test_factory.csv"