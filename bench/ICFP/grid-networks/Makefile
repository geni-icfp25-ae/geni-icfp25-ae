ifeq ($(SANITY_CHECK),1)
GENF_N := 5  # 19.61 mins
PLOG_N := 7  #  7.47 mins
DICE_N := 16 #  8.58 mins
GENI_N := 16 # 17.23 mins
ROUTER_DICE_N := 17 # 7.72 mins
ROUTER_GENI_N := 17 # 8.55 mins
else
GENF_N := 5
PLOG_N := 7
DICE_N := 18
GENI_N := 18
ROUTER_DICE_N := 20
ROUTER_GENI_N := 20
endif

link-failure.genfer.csv: bench.sh link-failure.genfer.py
	./bench.sh "genfer" link-failure 3-flow 1 ${GENF_N}

link-failure.problog.csv: bench.sh link-failure.problog.py
	./bench.sh problog link-failure 3-flow 1 ${PLOG_N}

link-failure.dice.csv: bench.sh link-failure.dice.py
	./bench.sh dice link-failure 3-flow 1 ${DICE_N} 

link-failure.gennifer.csv: bench.sh link-failure.gennifer.py
	./bench.sh "gennifer -i -l --pt" link-failure 3-flow 1 ${GENI_N}

link-failure.png: link-failure.genfer.csv link-failure.problog.csv link-failure.dice.csv link-failure.gennifer.csv 
	plot --x parameter_length --y mean \
		--csv link-failure.genfer.csv link-failure.problog.csv link-failure.dice.csv link-failure.gennifer.csv \
		--label genfer problog dice gennifer \
		--title "Grid Networks (Link Failure)" \
		--savefig link-failure.png

router-failure.dice.csv: bench.sh router-failure.dice.py
	./bench.sh dice router-failure 3-flow 1 ${ROUTER_DICE_N} 

router-failure.gennifer.csv: bench.sh router-failure.gennifer.py
	./bench.sh "gennifer -i -l --pt" router-failure 3-flow 1 ${ROUTER_GENI_N}

router-failure.png: router-failure.dice.csv router-failure.gennifer.csv 
	plot --x parameter_length --y mean \
		--csv router-failure.dice.csv router-failure.gennifer.csv \
		--label dice gennifer \
		--title "Grid Networks (Router Failure)" \
		--savefig router-failure.png

clean:
	rm -f link-failure.dice.csv 
	rm -f link-failure.gennifer.csv 
	rm -f link-failure.problog.csv
	rm -f link-failure.genfer.csv
	rm -f link-failure.png
	rm -f router-failure.dice.csv
	rm -f router-failure.gennifer.csv
	rm -f router-failure.png