all: dice-summary.csv gennifer-summary.csv

dice-summary.csv: bench.dice.sh aggregate_max_mean.py
	# SANITI_CHECK=1 warmup_runs=3 hyperf_runs=10 ~30min
	# SANITI_CHECK=0
	sh bench.dice.sh

gennifer-l-i--pt-summary.csv: bench.gennifer.sh aggregate_max_mean.py
	# SANITI_CHECK=1 warmup_runs=3 hyperf_runs=10 3.63 min
	sh bench.gennifer.sh

clean:
	rm -f dice-summary.csv
	rm -f gennifer-l-i--pt-summary.csv