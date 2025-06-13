#!/bin/bash
for file in $(ls c/*.c); do
    ../target/debug/gennifer --c -o tmp.c $(basename $file .c).gir > /dev/null
    gcc tmp.c -o tmp.out -lm -l:libmemoization.a -l:libmurmur_hash3.a
    sed -i 's/[^_]fn[0-9a-fx]\+//g' tmp.c
    diff <(sed 's/[^_]fn[0-9a-fx]\+//g' $file) tmp.c
    #we're really only looking for error messages here
    ./tmp.out > /dev/null
done;
