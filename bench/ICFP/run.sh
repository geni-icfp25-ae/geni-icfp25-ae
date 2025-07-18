#!/usr/bin/env bash

{
    echo "Running sanity checks..."
    echo "time SANITY_CHECK=1 make -C bayesian-networks dice-summary.csv"
    time SANITY_CHECK=1 make -C bayesian-networks dice-summary.csv
    echo "make -C bayesian-networks clean"
    make -C bayesian-networks clean

    export SANITY_CHECK=0    
    echo "Running benchmarks..."
    echo "SANITY_CHECK=${SANITY_CHECK}"

    time NUM_SAMPLES=30000 make -C poisson-packet-arrival poisson_packet_arrival_pyro.csv # 15.82 mins
    time make -C poisson-packet-arrival poisson_packet_arrival_geni.csv
    time make -C poisson-packet-arrival poisson_packet_arrival_pyro.png

    time make -C weather-factory bench.dice # 70m47.652s
    time make -C weather-factory bench.gennifer # 34m59.847s
    time make -C weather-factory test_horizon.png
    time make -C weather-factory test_factory.png

    time make -C grid-networks link-failure.genfer.csv # 20m30.931s
    time make -C grid-networks link-failure.problog.csv # 7m33.135s
    time make -C grid-networks link-failure.dice.csv # 161m28.805s
    time make -C grid-networks link-failure.gennifer.csv # 99m24.736s
    time make -C grid-networks link-failure.png

    time make -C grid-networks router-failure.dice.csv # 286m7.356s
    time make -C grid-networks router-failure.gennifer.csv # 117m42.930s
    time make -C grid-networks router-failure.png

    time make -C bayesian-networks dice-summary.csv # 68m49.763s
    time make -C bayesian-networks gennifer-l-i--pt-summary.csv #1030m41.729s

} 2>&1 | tee run.log
