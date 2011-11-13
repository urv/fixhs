benchmark: 
	@ghc --make -rtsopts -prof -auto-all -outputdir dist/build -O -optc-O3 -funfolding-use-threshold=16 Benchmark.hs -fforce-recomp

benchmark2: 
	@ghc --make -outputdir dist/build -O -optc-O3 -funfolding-use-threshold=16 Benchmark.hs -fforce-recomp

show:
	hp2ps -e8in -c Benchmark.hp
	gv Benchmark.ps
