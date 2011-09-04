all: configure
	cabal build

clean:
	@cabal clean
	@rm -f Benchmark
	@rm -f **/*.hi
	@rm -f **/*.o

configure:
	cabal configure

benchmark: configure
	@ghc --make -outputdir dist/build -O -optc-O3 -funfolding-use-threshold=16 Benchmark.hs -fforce-recomp
	./Benchmark
