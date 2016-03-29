
all: build

clean:
	rebar clean

build:
	rebar compile

check:
	rebar eunit
