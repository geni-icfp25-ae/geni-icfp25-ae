# Experimental Evaluation

Ensure you have the nix shell running by following the steps in the root README.

Verify all executables are install:
```console
> gennifer
error: the following required arguments were not provided: <FILE_NAME>
Usage: gennifer <FILE_NAME>
For more information, try '--help'.

> genfer
error: the following required arguments were not provided: <FILE_NAME>
Usage: genfer <FILE_NAME>
For more information, try '--help'.

> dice
Error parsing command line: issing anonymous argument: FILENAME
For usage information, run dice -help

> problog
ERROR: Expected a file or stream as input.
...
```

## table of contents
| Figures in the paper | folder                   | 
| -------------------- | ------------------------ | 
| Figure 9             | weather-factory          | 
| Figure 11            | grid-networks            |
| Table  1             | bayesian-networks        |
| Figure 12            | poisson-packet-arrival   |


All experiments were conducted on a workstation equipped with an **Intel(R) Core(TM) i7-7820X CPU @ 3.60GHz** and **128 GB** of RAM.

Change **working directory** to the `bench/icfp` folder before running the commands below.
```
(main)> tree . -L 1
.
├── bayesian-networks
├── grid-networks
├── poisson-packet-arrival
├── README.md
└── weather-factory
```

## Figure 9: weather-factory

-   Evaluate `dice` on the weather-factory benchmark:
    ```shell
    SANITY_CHECK=1 make -C weather-factory bench.dice # ~15 mins
    ```
    This will generate two files:
    - `weather-factory/dice.test_horizon.csv`, and
    - `weather-factory/dice.test_factory.csv`.
-   Evaluate `gennifer` on the weather-factory benchmark:
    ```shell
    SANITY_CHECK=1 make -C weather-factory bench.gennifer # ~15 mins
    ```
    This will generate two files:
    - `weather-factory/gennifer-l-i--pt.test_horizon.csv`, and
    - `weather-factory/gennifer-l-i--pt.test_factory.csv`.
-   ```shell
    make -C weather-factory test_horizon.png
    ```
    reproduces Figure 9(left), saved in `weather-factory/test_horizon.png`. 
-   ```shell
    make -C weather-factory test_factory.png
    ```
    reproduces Figure 9(right), saved in `weather-factory/test_factory.png`. 

## Figure 11: grid-networks

-   Evaluate `genfer` on the grid-networks(link-failure) benchmark:
    ```shell
    SANITY_CHECK=1 make -C grid-networks link-failure.genfer.csv # ~20 mins
    ```
    This will generate `grid-networks/link-failure.genfer.csv`.

-   Evaluate `problog` on the grid-networks(link-failure) benchmark:
    ```shell
    SANITY_CHECK=1 make -C grid-networks link-failure.problog.csv # ~10 mins
    ```
    This will generate `grid-networks/link-failure.problog.csv`.

-   Evaluate `dice` on the grid-networks(link-failure) benchmark:
    ```shell
    SANITY_CHECK=1 make -C grid-networks link-failure.dice.csv # ~10 mins
    ```
    This will generate `grid-networks/link-failure.dice.csv`.

-   Evaluate `gennifer` on the grid-networks(link-failure) benchmark:
    ```shell
    SANITY_CHECK=1 make -C grid-networks link-failure.gennifer.csv # ~20 mins
    ```
    This will generate `grid-networks/link-failure.gennifer.csv`.
-   ```shell
    make -C grid-networks link-failure.png
    ```
    reproduces Figure 11(left), saved in `grid-networks/link-failure.png`.

-   Evaluate `dice` on the grid-networks(router-failure) benchmark:
    ```shell
    SANITY_CHECK=1 make -C grid-networks router-failure.dice.csv # ~10 mins
    ```
    This will generate `grid-networks/router-failure.dice.csv`.

-   Evaluate `gennifer` on the grid-networks(router-failure) benchmark:
    ```shell
    SANITY_CHECK=1 make -C grid-networks router-failure.gennifer.csv # ~10 mins
    ```
    This will generate `grid-networks/router-failure.gennifer.csv`.
    
-   ```shell
    make -C grid-networks router-failure.png
    ```
    reproduces Figure 11(right), saved in `grid-networks/router-failure.png`.

## Table 1: bayesian-networks

-   Evaluate `dice` on the bayesian-networks benchmark:
    ```shell
    SANITY_CHECK=1 make -C bayesian-networks dice-summary.csv # ~5 mins
    ```
    This reproduces Table 1 (column Dice) saved in `bayesian-networks/dice-summary.csv`.

-   Evaluate `gennifer` on the bayesian-networks benchmark:
    ```shell
    SANITY_CHECK=1 make -C bayesian-networks gennifer-l-i--pt-summary.csv # ~5 mins
    ```
    This reproduces Table 1 (column Gennifer) saved in `bayesian-networks/gennifer-l-i--pt-summary.csv`.

## Figure 12: poisson-packet-arrival

-   Evaluate Pyro's importance sampling algorithm on the poisson-packet-arrival benchmark:
    ```shell
    NUM_SAMPLES=30000 make -C poisson-packet-arrival poisson_packet_arrival_pyro.csv # ~ 15 mins
    ```
    This will generate `poisson-packet-arrival/poisson_packet_arrival_pyro.csv`.

-   ```shell
    make -C poisson-packet-arrival poisson_packet_arrival_pyro.png
    ```
    reproduces the the line representing Pyro IS (shown in blue) in Figure 12, saved in `poisson-packet-arrival/poisson_packet_arrival_pyro.png`.

-   Evaluate `gennifer` on the poisson-packet-arrival benchmark:
    ```shell
    make -C poisson-packet-arrival poisson_packet_arrival_geni.csv #  < 1 min
    ```
    This will generate `poisson-packet-arrival/poisson_packet_arrival_geni.csv`, which represents Geni in Figure 12.

## Notes

`SANITY_CHECK=1` enables partial runs for faster reproduction of results. All benchmarks are included, but for each benchmark, only a subset of programs is executed—skipping a few especially time-consuming ones. For example, in the Bayesian network benchmark, `SANITY_CHECK=1` runs 5 networks, while `SANITY_CHECK=0` runs all 7. 

Set `SANITY_CHECK=0` to run the full set of programs in each benchmark, which may (1) take several days to complete and (2) require significant memory.










