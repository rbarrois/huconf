default: build

build: huconf

huconf: src/huconf.hs
	ghc -o $@ --make $<


TEST_CASES = $(wildcard tests/*.source)
TEST_RESULTS = $(patsubst tests/%.source,tests/work/%.result,$(TEST_CASES))
TEST_CATEGORIES := shell,x11,work

tests/work/%.computed: tests/%.source huconf
	mkdir -p $(@D)
	./huconf $< $(TEST_CATEGORIES) > $@

tests/work/%.result: tests/%.expected tests/work/%.computed
	-diff --unified $^ | tee $@

test: $(TEST_RESULTS)
	test -z $$(find $^ -not -empty)


clean:
	-rm huconf
	-rm -r tests/work/
