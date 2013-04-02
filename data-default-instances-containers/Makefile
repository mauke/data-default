TESTS = $(wildcard t/*.hs)

.PHONY: config build install docs dist test clean
.PHONY: build-test clean-test

build: dist/setup-config
	cabal build

config: dist/setup-config

dist/setup-config: *.cabal
	cabal configure

install: build
	cabal install

docs: dist/setup-config
	cabal haddock

dist: build
	cabal sdist

clean: clean-test
	cabal clean

test: build-test
	prove

build-test: $(TESTS:.hs=.t)

clean-test:
	rm -f $(TESTS:.hs=.t) $(TESTS:.hs=.hi) $(TESTS:.hs=.o)

t/%.t: t/%.hs build
	ghc -idist/build -Wall -O --make $< -o $@
