-module(emqtt_packet).

-include("emqtt.hrl").
%TODO:

encode(#mqtt_packet{} = Packet) ->
	<<>>.

decode(Data) when is_binary(Data) ->
	#mqtt_packet{}.

