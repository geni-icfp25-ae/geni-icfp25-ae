#!/bin/bash
for file in $(ls ocaml/*.ml); do
    ../target/debug/gennifer --ocaml -o tmp.ml $(basename $file .ml).gir > /dev/null
    ocaml tmp.ml > /dev/null
    sed -i 's/[^_]fn[0-9a-fx]\+//g' tmp.ml
    diff <(sed 's/[^_]fn[0-9a-fx]\+//g' $file) tmp.ml
    #we're really only looking for error messages here
done;
