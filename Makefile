#!/usr/bin/env sh
APP_NAME = pg_sql

all: reset start

dep:
	rm -rf ./deps/* && ./rebar get-deps

reset: clean compile generate

clean: 
	./rebar clean
compile:
	./rebar compile

generate:
	./rebar generate
	chmod 777 rel/$(APP_NAME)/bin/$(APP_NAME)

console:
	./rel/$(APP_NAME)/bin/$(APP_NAME) console
start:
	./rel/$(APP_NAME)/bin/$(APP_NAME) start
restart:
	./rel/$(APP_NAME)/bin/$(APP_NAME) restart
stop:
	./rel/$(APP_NAME)/bin/$(APP_NAME) stop
ping:
	./rel/$(APP_NAME)/bin/$(APP_NAME) ping
foreground:
	./rel/$(APP_NAME)/bin/$(APP_NAME) foreground
build_plt:
	dialyzer --build_plt --output_plt $(APP_NAME).plt --apps erts kernel stdlib crypto public_key ssl edoc -r deps 	
analyze: compile
	ERL_LIBS=deps dialyzer --plt $(APP_NAME).plt -r apps/$(APP_NAME)/ --src
test:
	./rebar skip_deps=true eunit
