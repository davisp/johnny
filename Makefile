
all: build

clean:
	rebar clean

build:
	rebar compile

check: build
	rebar eunit
