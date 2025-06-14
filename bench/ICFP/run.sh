make -C weather-factory bench.dice
make -C weather-factory bench.gennifer
make -C weather-factory test_horizon.png
make -C weather-factory test_factory.png

make -C grid-networks link-failure.genfer.csv
make -C grid-networks link-failure.problog.csv
make -C grid-networks link-failure.dice.csv
make -C grid-networks link-failure.gennifer.csv
make -C grid-networks link-failure.png

make -C grid-networks router-failure.dice.csv
make -C grid-networks router-failure.gennifer.csv
make -C grid-networks router-failure.png

make -C bayesian-networks dice-summary.csv
make -C bayesian-networks gennifer-l-i--pt-summary.csv

NUM_SAMPLES=30000 make -C poisson-packet-arrival poisson_packet_arrival_pyro.csv
make -C poisson-packet-arrival poisson_packet_arrival_pyro.png
make -C poisson-packet-arrival poisson_packet_arrival_geni.csv
