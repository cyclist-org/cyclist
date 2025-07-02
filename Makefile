ROOT := $(shell pwd)
DEFS := $(ROOT)/examples/sl.defs

export ROOT DEFS

all:
	dune build

clean:
	dune clean

%-tests:
	$(MAKE) -C benchmarks $*
