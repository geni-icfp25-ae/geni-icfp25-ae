# [**ICFP'25 Artifact**] Compiling with Generating Functions

This repository contains the tool source code, benchmarks and instructions to reproduce the results in the paper. 

## Table of Contents

* [Repo overview](#overview)
* [Platform requirements](#platform-requirements)
* [Getting started](#getting-started)
* [Experimental Evaluation](#experimental-evaluation)

## Overview

```console
> gennifer --help
Command-line argument parser

Usage: gennifer [OPTIONS] <FILE_NAME>

Arguments:
  <FILE_NAME>  Input file containing the source code

Options:
      --print-program            Print the parsed program
      --print-gf                 Print the compiled generating function
      --print-time               Print the time consumption
      --simplify                 
      --type-check               Type check the program
  -i, --interpret                Interpret the program
      --taylor                   Use the taylor expansion engine from Genfer to interpret the program WARRING: This is
                                 experimental and may not work as expected. WARNING: Only tested for the Possion Packet
                                 Arrival Benchmark
      --skip-table               Skip the table printing
  -l, --live                     Apply free variable analysis before compiling to generating functions
      --c                        Compile the generating function to a C program
      --ocaml                    Compile the generating function to a OCaml program
  -o, --output <OUTPUT>          Output file name
      --table-size <TABLE_SIZE>  Hash table size for the memoization
      --quiet                    Quiet level for warning messages
  -h, --help                     Print help
```

## Platform requirements

To set up the environment with all required dependencies, you will build the environment from scratch using [Nix](https://nixos.org/download). We uses Nix version 2.21.0. 
All experiments were conducted on a workstation equipped with an **Intel(R) Core(TM) i7-7820X CPU @ 3.60GHz** and **128 GB** of RAM.
This Nix environment is **not** for Apple ARM processors.

## Getting Started

### Set up the environment

#### Option 1. Using Nix

-   Install Nix by running the following command in your terminal:
    ```
    sh <(curl -L https://nixos.org/nix/install) --daemon
    ```

    Please refer to https://nixos.org/download for more info.

-   Enter the Nix shell by running:
    ```
    nix --experimental-features 'nix-command flakes' develop -i
    ```

-   Verify the nix shell is running by typing `which gennifer`. The output should be similar to:
    ```
    /nix/store/bc01gdm7lwjgh6cj2icnnlq7gwr022xa-gennifer-1.0/bin/gennifer
    ``` 
#### Option 2. Using Docker

-   Install Docker by following the instructions at https://docs.docker.com/get-docker/.

-   Build the Docker image from `Dockerfile` in the root directory of this repository by running:
    ```shell
    > docker build -t geni-icfp25-ae .
    [+] Building 1063.0s (19/19) FINISHED
    => [internal] load build definition from Dockerfile                                                                 0.2s
    => => transferring dockerfile: 681B                                                                                 0.0s
    => [internal] load metadata for docker.io/nixos/nix:latest                                                          1.7s
    => [internal] load .dockerignore                                                                                    0.2s
    => => transferring context: 2B                                                                                      0.0s
    => [ 1/14] FROM docker.io/nixos/nix:latest@sha256:016f07dddeb5feabeb75c360edb840ff4df3b89c7e0ca7ff1faea6240ce8375a 79.6s
    => [internal] load build context                                                                                    0.6s
    => => transferring context: 16.86MB                                                                                 0.3s
    => [ 2/14] RUN mkdir -p /etc/nix && echo "experimental-features = nix-command flakes" > /etc/nix/nix.conf           3.1s
    => [ 3/14] COPY . /geni-icfp25-ae                                                                                   0.7s
    => [ 4/14] WORKDIR /geni-icfp25-ae                                                                                  0.4s
    => [ 5/14] RUN nix build .#pyro-ppl                                                                                39.6s
    => [ 6/14] RUN nix build .#ocaml                                                                                    8.7s
    => [ 7/14] RUN nix build .#rustc                                                                                   23.2s
    => [ 8/14] RUN nix build .#cargo                                                                                    3.6s
    => [ 9/14] RUN nix build .#c-memo                                                                                   9.2s
    => [10/14] RUN nix build .#bngen                                                                                  247.1s
    => [11/14] RUN nix build .#dice                                                                                   141.2s
    => [12/14] RUN nix build .#problog                                                                                  9.2s
    => [13/14] RUN nix build .#genfer                                                                                 311.4s
    => [14/14] RUN nix build .#gennifer                                                                               151.2s
    => exporting to image                                                                                              31.7s
    => => exporting layers                                                                                             31.5s
    => => writing image sha256:20b55f86516ba2f119d4a68eff020731c2ca62427b1faabf6ec11b4054525fec                         0.0s
    => => naming to docker.io/library/geni-icfp25-ae                                                                    0.0s
    ```
    Building the Docker image takes approximately 20 minutes without cache, of which about 15 minutes are spent on `RUN nix build .#ddependencies` commands to build the Nix environment.

-   (optinal) Saving a clean docker image as `geni-icfp25-ae.tar` before running all the experiments.

    ```shell
    > docker save geni-icfp25-ae > geni-icfp25-ae.tar
    ```

-   run the docker image, enter the docker container, change the directory to `/geni-icfp25-ae/bench/ICFP`, and enter `nix-shell`, either

    ```shell
    > docker run -it geni-icfp25-ae #(and then detach by ctrl-p ctrl-q)
    > docker ps
    CONTAINER ID   IMAGE             COMMAND                  CREATED         STATUS         PORTS     NAMES
    45f79ecec6b4   geni-icfp25-ae    "nix --extra-experim…"   ....            ....           ....      ....
    > docker exec -it 45f7 bash -c "cd /geni-icfp25-ae/bench/ICFP && nix develop"
    ```
    or simply
    ```shell
    > docker run -it geni-icfp25-ae bash -c "cd /geni-icfp25-ae/bench/ICFP && nix develop"
    ```

-   Please refer to `geni-icfp25-ae/bench/ICFP/README.md` to run all the experiments in the `nix-shell`

-   (optional) save the docker image:

    ```shell
    > docker commit 45f7 geni-icfp25-ae-tested
    sha256:0183fd14aeba3969c387b4128927c9b4e5e1828acaca41430d93b821d76b7795
    
    > docker save geni-icfp25-ae-tested > geni-icfp25-ae-tested
    ```

## Experimental Evaluation

Benchmark scripts to reproduce results from the paper are at `bench/ICFP`.