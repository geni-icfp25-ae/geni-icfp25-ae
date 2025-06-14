# dice.test_horizon.csv: bench.dice
# dice.test_factory.csv: bench.dice
bench.dice: bench.dice.sh
	# SANITY_CHECK=1 warmup_runs=0 hyperf_runs=1 ~15min
	sh bench.dice.sh

# gennifer-l-i--pt.test_horizon.csv: bench.gennifer
# gennifer-l-i--pt.test_factory.csv: bench.gennifer
bench.gennifer: bench.gennifer.sh
	# SANITY_CHECK=1 warmup_runs=0 hyperf_runs=1 ~12min
	sh bench.gennifer.sh

test_horizon.png: dice.test_horizon.csv gennifer-l-i--pt.test_horizon.csv
	plot --label dice gennifer --y mean \
		--x parameter_h \
		--title "Weather Factory Test Horizon(n=10)" \
		--savefig test_horizon.png \
		--csv dice.test_horizon.csv gennifer-l-i--pt.test_horizon.csv \
		
test_factory.png: dice.test_factory.csv gennifer-l-i--pt.test_factory.csv
	plot --label dice gennifer --y mean \
		--x parameter_n \
		--title "Weather Factory Test #Factory(h=10)" \
		--savefig test_factory.png \
		--csv dice.test_factory.csv gennifer-l-i--pt.test_factory.csv \
	
plot: test_horizon.png test_factory.png

clean:
	rm -f dice.test_horizon.csv dice.test_factory.csv \
		gennifer-l-i--pt.test_horizon.csv gennifer-l-i--pt.test_factory.csv \
		test_horizon.png test_factory.png