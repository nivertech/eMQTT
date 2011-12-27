-module(emqtt_app)

-behavior(application).

-export([start/2,
		stop/1]).

start(normal, _) ->
	emqtt_sup:start_link().

stop(_) ->
	ok.
