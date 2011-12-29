-module(emqtt_packet).

-include("emqtt.hrl").

-export([encode/1, decode/1]).

encode(#mqtt_packet{} = _Packet) ->
	<<>>.

decode(Data) when is_binary(Data) ->
	#mqtt_packet{}.

