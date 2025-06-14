#!/usr/bin/env bash

here=$(dirname "$0")
EXEC="gennifer -l -i --pt"
folder_name=$(date +"%Y-%m-%d_%H-%M-%S")
path_to_folder="${here}/${folder_name}"
log_name="${EXEC// /}"

# Set SANITY_CHECK to 1 to enable, or leave unset/0 to disable
SANITY_CHECK=${SANITY_CHECK:-0}

if [[ "${SANITY_CHECK}" -eq 1 ]]; then
    warmup_runs=3
    hyperf_runs=10
else
    warmup_runs=3
    hyperf_runs=10
fi

# Use user-provided networks or fall back to default
if [[ "$#" -gt 0 ]]; then
    bn_list=("$@")
elif [[ "$SANITY_CHECK" -eq 1 ]]; then
    bn_list=(alarm insurance hepar2 hailfinder pigs)
else
    bn_list=(alarm insurance hepar2 hailfinder pigs water munin)
fi

mkdir "${path_to_folder}"

{
    echo "Benchmarking Bayesian networks with the following settings:"
    echo "SANITY_CHECK: ${SANITY_CHECK}"
    echo "Using Bayesian networks: ${bn_list[*]}"
    echo "Warmup runs: ${warmup_runs}, Hyperfine runs: ${hyperf_runs}"
    echo "Output folder: ${path_to_folder}"

    csv_files=()
    for bn in "${bn_list[@]}"; do
        file="${here}/bif/${bn}.bif"
        filename=$(basename "$file")
        size=$(bngen size "$file")
        echo "$filename constains $size nodes: 0..$((size - 1))"
        echo "${prefix}${suffix}"
        dice_source="${path_to_folder}/${filename}.{length}.dice"
        gir_source="${dice_source}.gir"

        # Skip printing .dice and .gir 
        # this .dice and .gir are used for debugging 
        # bngen sym "${file}" | tee "${path_to_folder}/${filename}.dice"
        # dice "${path_to_folder}/${filename}.dice" -show-internal-gennifer-nowrap -skip-table | tee "${path_to_folder}/${filename}.dice.gir"
        
        hyperfine_export_csv="${path_to_folder}/${log_name}.${filename}.csv"
        csv_files+=("${hyperfine_export_csv}")
        hyperfine --export-csv ${hyperfine_export_csv} \
            --show-output \
            -w ${warmup_runs} \
            -r ${hyperf_runs} \
            --parameter-scan length 0 "$((size - 1))" \
            --setup "bngen single ${file} {length} > ${dice_source} && dice ${dice_source} -show-internal-gennifer-nowrap -skip-table > ${gir_source} && bngen order ${file} {length}" \
            --cleanup "rm ${dice_source} ${gir_source}" \
            "${EXEC} ${gir_source}"
    done

    python3 "${here}/aggregate_max_mean.py" \
        --inputs "${csv_files[@]}" \
        --output "${here}/${log_name}-summary.csv"
} 2>&1 | tee "${path_to_folder}/${log_name}.log"
