#!/usr/bin/env bash

{
    echo "Running sanity checks..."
    echo "time SANITY_CHECK=1 make -C bayesian-networks dice-summary.csv"
    time SANITY_CHECK=1 make -C bayesian-networks dice-summary.csv
    echo "make -C bayesian-networks clean"
    make -C bayesian-networks clean
    
    echo "Running benchmarks..."
    echo "SANITY_CHECK=${SANITY_CHECK}"

    time make -C weather-factory bench.dice
    time make -C weather-factory bench.gennifer
    time make -C weather-factory test_horizon.png
    time make -C weather-factory test_factory.png

    time make -C grid-networks link-failure.genfer.csv
    time make -C grid-networks link-failure.problog.csv
    time make -C grid-networks link-failure.dice.csv
    time make -C grid-networks link-failure.gennifer.csv
    time make -C grid-networks link-failure.png

    time make -C grid-networks router-failure.dice.csv
    time make -C grid-networks router-failure.gennifer.csv
    time make -C grid-networks router-failure.png

    time make -C bayesian-networks dice-summary.csv
    time make -C bayesian-networks gennifer-l-i--pt-summary.csv

    time NUM_SAMPLES=30000 make -C poisson-packet-arrival poisson_packet_arrival_pyro.csv
    time make -C poisson-packet-arrival poisson_packet_arrival_pyro.png
    time make -C poisson-packet-arrival poisson_packet_arrival_geni.csv
} 2>&1 | tee run.log
