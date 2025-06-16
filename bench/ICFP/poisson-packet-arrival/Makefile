# NUM_SAMPLES=30000 15.58 mins 

poisson_packet_arrival_pyro.csv: poisson_packet_arrival.py
	python -u poisson_packet_arrival.py \
		--model observation \
		--inference IS \
		--r 10 \
		--p 0.6170807 \
		--seed 1984 \
		--num-samples ${NUM_SAMPLES} | tee poisson_packet_arrival_pyro.csv

poisson_packet_arrival_pyro.png: poisson_packet_arrival_pyro.csv
	plot --x time --y l1 --label pyro \
		--csv poisson_packet_arrival_pyro.csv \
		--title "Poisson Packet Arrival" \
		--save poisson_packet_arrival_pyro.png 

poisson_packet_arrival_geni.csv:
	hyperfine "bench-poisson-packet-arrival-link-failure-3-3" \
		--export-csv poisson_packet_arrival_geni.csv \
		--warmup 100 \
		-m 5000

clean: 
	rm -f poisson_packet_arrival_pyro.csv poisson_packet_arrival_pyro.png poisson_packet_arrival_geni.csv