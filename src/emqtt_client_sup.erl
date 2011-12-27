-module(emqtt_client_sup).

-behavior(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{simple_one_for_one, 3, 1},
          [{undefined, {emqtt_client, start_link, []},
              transient, 10, worker, [emqtt_client]}]}}.


