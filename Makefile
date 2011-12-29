
all: compile

compile:
	./rebar compile

clean:
	./rebar clean

eunit:
	./rebar skip_deps=true eunit

dialyzer: compile
	@dialyzer -Wno_return -c src/apps/emqtt/ebin


