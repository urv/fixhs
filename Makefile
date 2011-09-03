clean:
	@cabal clean
	@rm -f Benchmark
	@rm -f **/*.hi
	@rm -f **/*.o

conf:
	cabal configure

benchmark: conf
	@ghc --make -outputdir dist/build -O -optc-O3 -funfolding-use-threshold=16 Benchmark.hs -fforce-recomp
	./Benchmark
