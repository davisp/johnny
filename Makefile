
TEST_MODULES = \
    test/etap.beam \
    test/gen_term.beam \
    test/util.beam

all: build

clean:
	./rebar clean
	rm -rf logs
	rm -rf .eunit
	rm test/*.beam

deps: ./deps/
	./rebar get-deps update-deps


build: deps
	./rebar compile


etap: $(TEST_MODULES)
	prove test/*.t


eunit:
	./rebar eunit skip_deps=true


check: etap eunit


%.beam: %.erl
	erlc -o test/ $<

