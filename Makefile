
TEST_MODULES = \
    test/etap.beam \
    test/gen_term.beam \
    test/util.beam

all: build

clean:
	./rebar clean
	rm -rf logs
	rm test/*.beam

deps: ./deps/
	./rebar get-deps update-deps


build: deps
	./rebar compile


etap: $(TEST_MODULES)
	prove test/*.t


check: etap


%.beam: %.erl
	erlc -o test/ $<

