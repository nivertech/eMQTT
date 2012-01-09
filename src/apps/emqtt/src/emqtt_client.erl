-module(emqtt_client).

-include("emqtt.hrl").

-export([start_link/1]).

-behaviour(gen_server2).

-record(state, {sock}).

-export([init/1,
		handle_call/3,
	    handle_cast/2,
		handle_info/2,
		terminate/2,
		code_change/3
		%handle_pre_hibernate/1,
		%prioritise_call/3,
        %prioritise_cast/2,
		%prioritise_info/2
		]).


-spec start_link(term())->{ok,pid()} | ignore | {error,term()}.

start_link(Sock) -> 
	gen_server2:start_link(?MODULE, [Sock], []).

init([Sock]) ->
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
    {ok, #state{sock = Sock}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.
%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(#mqtt_packet{type = ?CONNECT}, #state{sock = Sock} = State) ->
	send(#mqtt_packet{type = ?CONNACK, arg = 0}, Sock),
    {noreply, State};

handle_cast(#mqtt_packet{type = ?PUBLISH, arg={Topic, Msg}}, State) ->
	emqtt_router:publish(Topic, Msg),
    {noreply, State};

handle_cast(#mqtt_packet{type = ?SUBSCRIBE, arg=Subs}, #state{sock = Sock} = State) ->
	[emqtt_router:subscribe(Topic, self()) ||
		#sub{topic = Topic} <- Subs],
	MsgId = random:uniform(16#FFFF),
	send(#mqtt_packet{type = ?SUBACK, arg={MsgId, Subs}}, Sock),
    {noreply, State};

handle_cast(#mqtt_packet{type = ?PINGREQ}, #state{sock = Sock} = State) ->
	send(#mqtt_packet{type = ?PINGRESP}, Sock),
    {noreply, State};

handle_cast(#mqtt_packet{type = ?DISCONNECT}, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
	io:format("badmsg: ~p~n", [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({publish, Topic, Msg}, #state{sock = Sock} = State) ->
	send(#mqtt_packet{type = ?PUBLISH, arg={Topic, Msg}}, Sock),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
	emqtt_router:unsubscribe(self()),
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

send(#mqtt_packet{} = Message, Socket) ->
  io:format("Message Sent: ~p~n", [Message]),
%%?LOG({mqtt_core, send, pretty(Message)}),
  {VariableHeader, Payload} = emqtt_packet:encode_message(Message),
  ok = send(emqtt_packet:encode_fixed_header(Message), Socket),
  ok = send_length(size(VariableHeader) + size(Payload), Socket),
  ok = send(VariableHeader, Socket),
  ok = send(Payload, Socket);
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



send_length(Length, Socket) when Length div 128 > 0 ->
  Digit = Length rem 128,
  send(<<1:1, Digit:7/big>>, Socket),
  send_length(Length div 128, Socket);
send_length(Length, Socket) ->
  Digit = Length rem 128,
  send(<<0:1, Digit:7/big>>, Socket).



