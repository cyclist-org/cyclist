all: central vars rules arity recs
central: sample-central.dat
vars: $(patsubst %, sample-vars-%.dat, 20 30 40 60 70 80)
rules: $(patsubst %, sample-rules-%.dat, 2-2 3-2 2-3 4-3 3-4 4-4)
arity: $(patsubst %, sample-arity-%.dat, 1 2 4 5)
recs: $(patsubst %, sample-recs-%.dat, 0 1 3 4)

sample-central.dat: run_sample.pl syn.pl
	./run_sample.pl | tee sample-central.dat

sample-vars-%.dat: run_sample.pl syn.pl
	./run_sample.pl -- -vars $* | tee $@

sample-rules-%.dat: run_sample.pl syn.pl
	./run_sample.pl -- -rules $* | tee $@

sample-arity-%.dat: run_sample.pl syn.pl
	./run_sample.pl -- -arity $* | tee $@

sample-recs-%.dat: run_sample.pl syn.pl
	./run_sample.pl -- -recurs $* | tee $@

clean:
	rm -f sample-*.dat

