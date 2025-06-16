#!/usr/bin/env bash

here=$(dirname "$0")
EXEC=$1
TYPE=$2 
MODE=$3 
STEP=$4
RANGE=${5:-10000000}
NCOL=${6:-10}

LANG="${EXEC%% *}"
file_name="${TYPE}.${LANG}"
log_name="${MODE}-${EXEC}"
log_name="${log_name// /}"

folder_name=$(date +"%Y-%m-%d_%H-%M-%S")
prefix="${here}/${folder_name}/${file_name}"

mkdir "${here}/${folder_name}"

{
    # Define setup, cleanup, and input file path pattern based on TYPE
    if [[ "$TYPE" == "link-failure" ]]; then
        python ${here}/${file_name}.py --mode ${MODE} --p 0.5 --n 10 --m 10 | tee ${prefix}.${MODE}.10.10.demo.${LANG}
        generate_prog_cmd="python ${here}/${file_name}.py --mode ${MODE} --m {length} --n {length} --p 0.5"
    elif [[ "$TYPE" == "router-failure" ]]; then
        python ${here}/${file_name}.py --m 10 --n 10 --p-init 0.5 --p-edge 0.5000001 0.5000002 --p-inner 0.5000003 0.5000004 0.5000005 0.5000006 | tee ${prefix}.${MODE}.10.10.demo.${LANG}
        generate_prog_cmd="python ${here}/${file_name}.py --m {length} --n {length} --p-init 0.5 --p-edge 0.5000001 0.5000002 --p-inner 0.5000003 0.5000004 0.5000005 0.5000006"
    else
        echo "Unknown TYPE: $TYPE"
        exit 1
    fi

    hyperfine --export-csv "${prefix}-${log_name}.csv" \
            --show-output \
            --runs 1 \
            --parameter-scan length 1 "${RANGE}" -D "${STEP}" \
            --setup "${generate_prog_cmd} > ${prefix}.${MODE}.{length}.{length}.${LANG}" \
            --cleanup "rm ${prefix}.${MODE}.{length}.{length}.${LANG}" \
            "$EXEC ${prefix}.${MODE}.{length}.{length}.${LANG}" \

    cp "${prefix}-${log_name}.csv" "${here}/${file_name}.csv"

} 2>&1 | tee "${prefix}-${log_name}.log"