%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ.
%%
%% The Initial Developer of the Original Code is VMware, Inc.
%% Copyright (c) 2007-2011 VMware, Inc.  All rights reserved.
%%
-module(emqtt_reader).

-include("emqtt.hrl").

-export([start_link/2, shutdown/2]).

-export([init/3]).

-define(HANDSHAKE_TIMEOUT, 10).
-define(NORMAL_TIMEOUT, 3).
-define(CLOSING_TIMEOUT, 1).
-define(CHANNEL_TERMINATION_TIMEOUT, 3).
-define(SILENT_CLOSE_DELAY, 3).

%-record(v31, {parent, clientpid, sock}).

-ifdef(use_specs).

-spec(start_link/2 :: (port(), pid()) -> emqtt_types:ok(pid())).

-spec(shutdown/2 :: (pid(), string()) -> 'ok').

%% These specs only exists to add no_return() to keep dialyzer happy
-spec(init/3 :: (pid(), port(), pid()) -> no_return()).

-endif.

%%--------------------------------------------------------------------------

start_link(Sock, ClientPid) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [self(), Sock, ClientPid])}.

shutdown(Pid, Explanation) ->
    gen_server:call(Pid, {shutdown, Explanation}, infinity).

init(_Parent, Socket, ClientPid) ->
    process_flag(trap_exit, true),
    {PeerAddress, PeerPort} = socket_op(Socket, fun emqtt_net:peername/1),
    PeerAddressS = emqtt_misc:ntoab(PeerAddress),
    emqtt_log:info("starting TCP connection ~p from ~s:~p~n",
                    [self(), PeerAddressS, PeerPort]),
    erlang:send_after(?HANDSHAKE_TIMEOUT * 1000, self(),
                      handshake_timeout),
    %State = #v31{parent              = Parent,
    %            sock                = Socket,
    %            clientpid = ClientPid},
    try
        recvloop(Socket, ClientPid)
    catch
        Ex -> (if Ex == connection_closed_abruptly ->
                       fun emqtt_log:warning/2;
                  true ->
                       fun emqtt_log:error/2
               end)("exception on TCP connection ~p from ~s:~p~n~p~n",
                    [self(), PeerAddressS, PeerPort, Ex])
    after
        emqtt_log:info("closing TCP connection ~p from ~s:~p~n",
                        [self(), PeerAddressS, PeerPort])
        %% We don't close the socket explicitly. The reader is the
        %% controlling process and hence its termination will close
        %% the socket. Furthermore, gen_tcp:close/1 waits for pending
        %% output to be sent, which results in unnecessary delays.
        %%
        %% gen_tcp:close(ClientSock),
		%%TODO: FIXME
        %%emqtt_event:notify(connection_closed, [{pid, self()}])
    end,
    done.

socket_op(Sock, Fun) ->
    case Fun(Sock) of
        {ok, Res}       -> Res;
        {error, Reason} -> emqtt_log:error("error on TCP connection ~p:~p~n",
                                            [self(), Reason]),
                           emqtt_log:info("closing TCP connection ~p~n",
                                           [self()]),
                           exit(normal)
    end.

recvloop(Socket, ClientPid) ->
  FixedHeader = recv(1, Socket),
  io:format("fixed header: ~p~n", [FixedHeader]),
  RemainingLength = recv_length(Socket),
  io:format("Length: ~p~n", [RemainingLength]),
  Rest = recv(RemainingLength, Socket),
  Header = emqtt_packet:decode_fixed_header(FixedHeader),
  Message = emqtt_packet:decode_message(Header, Rest),
  io:format("Message: ~p~n", [Message]),
  gen_server:cast(ClientPid, Message),
  recvloop(Socket, ClientPid).

recv_length(Socket) ->
  recv_length(recv(1, Socket), 1, 0, Socket).
recv_length(<<0:1, Length:7>>, Multiplier, Value, _Socket) ->
  Value + Multiplier * Length;
recv_length(<<1:1, Length:7>>, Multiplier, Value, Socket) ->
  recv_length(recv(1, Socket), Multiplier * 128, Value + Multiplier * Length, Socket).

send_length(Length, Socket) when Length div 128 > 0 ->
  Digit = Length rem 128,
  send(<<1:1, Digit:7/big>>, Socket),
  send_length(Length div 128, Socket);
send_length(Length, Socket) ->
  Digit = Length rem 128,
  send(<<0:1, Digit:7/big>>, Socket).

recv(0, _Socket) ->
  <<>>;
recv(Length, Socket) ->
  case gen_tcp:recv(Socket, Length) of
    {ok, Bytes} ->
      Bytes;
    {error, Reason} ->
      ?LOG({recv, socket, error, Reason}),
      exit(Reason)
  end.

send(#mqtt_packet{} = Message, Socket) ->
%%?LOG({mqtt_packet_core, send, pretty(Message)}),
  {VariableHeader, Payload} = emqtt_packet:encode_message(Message),
  ok = send(emqtt_packet:encode_fixed_header(Message), Socket),
  ok = send_length(size(VariableHeader) + size(Payload), Socket),
  ok = send(VariableHeader, Socket),
  ok = send(Payload, Socket),
  ok;
send(<<>>, _Socket) ->
%%?LOG({send, no_bytes}),
  ok;
send(Bytes, Socket) when is_binary(Bytes) ->
%%?LOG({send,bytes,binary_to_list(Bytes)}),
  case gen_tcp:send(Socket, Bytes) of
    ok ->
      ok;
    {error, Reason} ->
      ?LOG({send, socket, error, Reason}),
      exit(Reason)
  end.

pretty(Message) when is_record(Message, mqtt_packet) ->
  lists:flatten(
    io_lib:format(
      "<matt id=~w type=~w (~w) dup=~w qos=~w retain=~w arg=~w>", 
      [Message#mqtt_packet.id, Message#mqtt_packet.type, emqtt_packet:command_for_type(Message#mqtt_packet.type), 
		Message#mqtt_packet.dup, Message#mqtt_packet.qos, Message#mqtt_packet.retain, Message#mqtt_packet.arg]
    )
  ).

default_client_id() ->
  {{_Y,Mon,D},{H,Min,S}} = erlang:localtime(),
  lists:flatten(io_lib:format(
    "~2.10.0B~2.10.0B~2.10.0B~2.10.0B~2.10.0B~w",
    [Mon, D, H, Min, S, self()]
  )).

