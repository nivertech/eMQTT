-module(emqtt_packet).

-author(ery.lee@gmail.com).

%%
%% An erlang client for MQTT (http://www.mqtt.org/)
%%
-include_lib("emqtt.hrl").

-compile(export_all).

-export([set_connect_options/1, set_publish_options/1, decode_message/2, encode_message/1]).

set_connect_options(Options) ->
  set_connect_options(Options, #connect_options{}).
set_connect_options([], Options) ->
  Options;
set_connect_options([{keepalive, KeepAlive}|T], Options) ->
  set_connect_options(T, Options#connect_options{keepalive = KeepAlive});
set_connect_options([{retry, Retry}|T], Options) ->
  set_connect_options(T, Options#connect_options{retry = Retry});
set_connect_options([{client_id, ClientId}|T], Options) ->
  set_connect_options(T, Options#connect_options{client_id = ClientId});
set_connect_options([{clean_start, Flag}|T], Options) ->
  set_connect_options(T, Options#connect_options{clean_start = Flag});
set_connect_options([{connect_timeout, Timeout}|T], Options) ->
  set_connect_options(T, Options#connect_options{connect_timeout = Timeout});
set_connect_options([#will{} = Will|T], Options) ->
  set_connect_options(T, Options#connect_options{will = Will});
set_connect_options([UnknownOption|_T], _Options) ->
  exit({connect, unknown_option, UnknownOption}).

set_publish_options(Options) ->
  set_publish_options(Options, #publish_options{}).
set_publish_options([], Options) ->
  Options;
set_publish_options([{qos, QoS}|T], Options) when QoS >= 0, QoS =< 2 ->
  set_publish_options(T, Options#publish_options{qos = QoS});
set_publish_options([{retain, true}|T], Options) ->
  set_publish_options(T, Options#publish_options{retain = 1});
set_publish_options([{retain, false}|T], Options) ->
  set_publish_options(T, Options#publish_options{retain = 0});
set_publish_options([UnknownOption|_T], _Options) ->
  exit({unknown, publish_option, UnknownOption}).

decode_message(#mqtt_packet{type = ?CONNECT} = Message, Rest) ->
  <<ProtocolNameLength:16/big, _/binary>> = Rest,
  {VariableHeader, Payload} = split_binary(Rest, 2 + ProtocolNameLength + 4),
  <<_:16, ProtocolName:ProtocolNameLength/binary, ProtocolVersion:8/big, _:2, WillRetain:1, WillQoS:2/big, WillFlag:1, CleanStart:1, _:1, KeepAlive:16/big>> = VariableHeader,
  {ClientId, Will} = case WillFlag of
    1 ->
      [C, WT, WM] = decode_strings(Payload),
      W = #will{
        topic = WT,
        message = WM,
        publish_options = #publish_options{qos = WillQoS, retain = WillRetain}
      },
      {C, W};
    0 ->
      [C] = decode_strings(Payload),
      {C, undefined}
  end,
  Message#mqtt_packet{
    arg = #connect_options{
      client_id = ClientId,
      protocol_name = binary_to_list(ProtocolName),
      protocol_version = ProtocolVersion,
      clean_start = CleanStart =:= 1,
      will = Will,
      keepalive = KeepAlive
    }
  };
decode_message(#mqtt_packet{type = ?CONNACK} = Message, Rest) ->
  <<_:8, ResponseCode:8/big>> = Rest,
  Message#mqtt_packet{arg = ResponseCode};
decode_message(#mqtt_packet{type = ?PINGRESP} = Message, _Rest) ->
  Message;
decode_message(#mqtt_packet{type = ?PINGREQ} = Message, _Rest) ->
  Message;
decode_message(#mqtt_packet{type = ?PUBLISH, qos = 0} = Message, Rest) ->
  {<<TopicLength:16/big>>, _} = split_binary(Rest, 2),
  {<<_:16, Topic/binary>>, Payload} = split_binary(Rest, 2 + TopicLength),
  Message#mqtt_packet{
    arg = {binary_to_list(Topic), binary_to_list(Payload)}
  };
decode_message(#mqtt_packet{type = ?PUBLISH} = Message, Rest) ->
  {<<TopicLength:16/big>>, _} = split_binary(Rest, 2),
  {<<_:16, Topic:TopicLength/binary, MessageId:16/big>>, Payload} = split_binary(Rest, 4 + TopicLength),
   Message#mqtt_packet{
    id = MessageId,
    arg = {binary_to_list(Topic), binary_to_list(Payload)}
  };
decode_message(#mqtt_packet{type = Type} = Message, Rest)
    when
      Type =:= ?PUBACK;
      Type =:= ?PUBREC;
      Type =:= ?PUBREL;
      Type =:= ?PUBCOMP ->
  <<MessageId:16/big>> = Rest,
  Message#mqtt_packet{
    arg = MessageId
  };
decode_message(#mqtt_packet{type = ?SUBSCRIBE} = Message, Rest) ->
  {<<MessageId:16/big>>, Payload} = split_binary(Rest, 2),
  Message#mqtt_packet{
    id = MessageId,
    arg = decode_subs(Payload, [])
  };
decode_message(#mqtt_packet{type = ?SUBACK} = Message, Rest) ->
  {<<MessageId:16/big>>, Payload} = split_binary(Rest, 2),
  GrantedQoS  = lists:map(fun(Item) ->
      <<_:6, QoS:2/big>> = <<Item>>,
      QoS
    end,
    binary_to_list(Payload)
  ),
  Message#mqtt_packet{
    arg = {MessageId, GrantedQoS}
  };
decode_message(#mqtt_packet{type = ?UNSUBSCRIBE} = Message, Rest) ->
  {<<MessageId:16/big>>, Payload} = split_binary(Rest, 2),
  Message#mqtt_packet{
    id = MessageId,
    arg = {MessageId, lists:map(fun(T) -> #sub{topic = T} end, decode_strings(Payload))}
  };
decode_message(#mqtt_packet{type = ?UNSUBACK} = Message, Rest) ->
  <<MessageId:16/big>> = Rest,
  Message#mqtt_packet{
    arg = MessageId
  };
decode_message(#mqtt_packet{type = ?DISCONNECT} = Message, _Rest) ->
  Message;
decode_message(Message, Rest) ->
  exit({decode_message, unexpected_message, {Message, Rest}}).

decode_subs(<<>>, Subs) ->
  lists:reverse(Subs);
decode_subs(Bytes, Subs) ->
  <<TopicLength:16/big, _/binary>> = Bytes,
  <<_:16, Topic:TopicLength/binary, ?UNUSED:6, QoS:2/big, Rest/binary>> = Bytes,
  decode_subs(Rest, [#sub{topic = binary_to_list(Topic), qos = QoS}|Subs]). 

encode_message(#mqtt_packet{type = ?CONNACK, arg = ReturnCode}) ->
  {<<?UNUSED:8, ReturnCode:8/big>>,<<>>};
encode_message(#mqtt_packet{type = ?CONNECT, arg = Options}) ->
  CleanStart = case Options#connect_options.clean_start of
    true ->
      1;
    false ->
      0
  end,
  {WillFlag, WillQoS, WillRetain, Payload} = case Options#connect_options.will of
    {will, WillTopic, WillMessage, WillOptions} ->
      O = set_publish_options(WillOptions),
      {
        1, O#publish_options.qos, O#publish_options.retain,
        list_to_binary([encode_string(Options#connect_options.client_id), encode_string(WillTopic), encode_string(WillMessage)])
      }; 
    undefined ->
      {0, 0, 0, encode_string(Options#connect_options.client_id)}
  end,
  {
    list_to_binary([
      encode_string(Options#connect_options.protocol_name),
      <<(Options#connect_options.protocol_version)/big>>, 
      <<?UNUSED:2, WillRetain:1, WillQoS:2/big, WillFlag:1, CleanStart:1, ?UNUSED:1, (Options#connect_options.keepalive):16/big>>
    ]),
    Payload
  };
encode_message(#mqtt_packet{type = ?PUBLISH, arg = {Topic, Payload}} = Message) ->
  if
    Message#mqtt_packet.qos =:= 0 ->
        {
          encode_string(Topic),
          encode_string(Payload)
        };
    Message#mqtt_packet.qos > 0 ->
        {
          list_to_binary([encode_string(Topic), <<(Message#mqtt_packet.id):16/big>>]),
          encode_string(Payload)
        }
  end;
encode_message(#mqtt_packet{type = ?PUBACK, arg = MessageId}) ->
  {
    <<MessageId:16/big>>,
    <<>>
  };
encode_message(#mqtt_packet{type = ?SUBSCRIBE, arg = Subs} = Message) ->
  {
    <<(Message#mqtt_packet.id):16/big>>,
    list_to_binary( lists:flatten( lists:map(fun({sub, Topic, RequestedQoS}) -> [encode_string(Topic), <<?UNUSED:6, RequestedQoS:2/big>>] end, Subs)))
  };
encode_message(#mqtt_packet{type = ?SUBACK, arg = {MessageId, Subs}}) ->
  {
    <<MessageId:16/big>>,
    list_to_binary(lists:map(fun(S) -> <<?UNUSED:6, (S#sub.qos):2/big>> end, Subs))
  }; 
encode_message(#mqtt_packet{type = ?UNSUBSCRIBE, arg = Subs} = Message) ->
  {
    <<(Message#mqtt_packet.id):16/big>>,
    list_to_binary(lists:map(fun({sub, T, _Q}) -> encode_string(T) end, Subs))
  };
encode_message(#mqtt_packet{type = ?UNSUBACK, arg = MessageId}) ->
  {<<MessageId:16/big>>, <<>>}; 
encode_message(#mqtt_packet{type = ?PINGREQ}) ->
  {<<>>, <<>>};
encode_message(#mqtt_packet{type = ?PINGRESP}) ->
  {<<>>, <<>>};
encode_message(#mqtt_packet{type = ?PUBREC, arg = MessageId}) ->
  {<<MessageId:16/big>>, <<>>};
encode_message(#mqtt_packet{type = ?PUBREL, arg = MessageId}) ->
  {<<MessageId:16/big>>, <<>>};
encode_message(#mqtt_packet{type = ?PUBCOMP, arg = MessageId}) ->
  {<<MessageId:16/big>>, <<>>};
encode_message(#mqtt_packet{type = ?DISCONNECT}) ->
  {<<>>, <<>>};
encode_message(#mqtt_packet{} = Message) ->
  exit({encode_message, unknown_type, Message}).


encode_fixed_header(Message) when is_record(Message, mqtt_packet) ->
  <<(Message#mqtt_packet.type):4/big, (Message#mqtt_packet.dup):1, (Message#mqtt_packet.qos):2/big, (Message#mqtt_packet.retain):1>>.

decode_fixed_header(Byte) ->
  <<Type:4/big, Dup:1, QoS:2/big, Retain:1>> = Byte,
  #mqtt_packet{type = Type, dup = Dup, qos = QoS, retain = Retain}.
 
command_for_type(Type) ->
  case Type of
    ?CONNECT -> connect;
    ?CONNACK -> connack;
    ?PUBLISH -> publish;
    ?PUBACK  -> puback;
    ?PUBREC -> pubrec;
    ?PUBREL -> pubrel;
    ?PUBCOMP -> pubcomp;
    ?SUBSCRIBE -> subscribe;
    ?SUBACK -> suback;
    ?UNSUBSCRIBE -> unsubscribe;
    ?UNSUBACK -> unsuback;
    ?PINGREQ -> pingreq;
    ?PINGRESP -> pingresp;
    ?DISCONNECT -> disconnect;
    _ -> unknown
  end.
 
encode_string(String) ->
  Bytes = list_to_binary(String),
  Length = size(Bytes),
  <<Length:16/big, Bytes/binary>>.

decode_strings(Bytes) when is_binary(Bytes) ->
  decode_strings(Bytes, []).
decode_strings(<<>>, Strings) ->
  lists:reverse(Strings);
decode_strings(<<Length:16/big, _/binary>> = Bytes, Strings) ->
  <<_:16, Binary:Length/binary, Rest/binary>> = Bytes,
  decode_strings(Rest, [binary_to_list(Binary)|Strings]).

