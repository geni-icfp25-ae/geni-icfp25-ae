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

NUM_SAMPLES=30000 time make -C poisson-packet-arrival poisson_packet_arrival_pyro.csv
time make -C poisson-packet-arrival poisson_packet_arrival_pyro.png
time make -C poisson-packet-arrival poisson_packet_arrival_geni.csv
