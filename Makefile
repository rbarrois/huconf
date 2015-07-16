default: build

build: huconf

huconf: src/huconf.hs
	ghc -o $@ --make $<

clean:
	@rm huconf
