%%%----------------------------------------------------------------------
%%% File    : emqtt_router.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : 
%%% Created : 03 Apr. 2010
%%% License : http://www.monit.cn/license
%%%
%%% Copyright (C) 2007-2010, www.monit.cn
%%%----------------------------------------------------------------------
-module(emqtt_router).

-author('ery.lee@gmail.com').

-include("emqtt.hrl").

-export([boot/0,
		dump/0,
		subscribe/2,
		unsubscribe/1,
		unsubscribe/2,
		publish/2]).

-behavior(gen_server2).

-export([start_link/0]).

-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-record(state, {}).

boot() ->
	{ok, _} = supervisor:start_child(
			emqtt_sup, 
			{emqtt_router, 
			 {emqtt_router, start_link, []}, 
			 permanent, infinity, worker, [emqtt_router]}
	),
	ok.
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server2:start_link({local, ?MODULE}, ?MODULE, [], []).


dump() ->
	Keys = mnesia:dirty_all_keys(topic),
	Topics = lists:flatten([mnesia:dirty_read(topic, Key) || Key <- Keys]), 
	[io:format("topic: ~p, sub: ~p~n", [Name, Pid]) 
		|| #topic{name = Name, sub = Pid} <- Topics].

subscribe(Topic, Pid) ->
	gen_server2:call(?MODULE, {subscribe, Topic, Pid}).

unsubscribe(Topic, Pid) ->
	gen_server2:cast(?MODULE, {unsubscribe, Topic, Pid}).

unsubscribe(Pid) ->
	gen_server2:cast(?MODULE, {unsubscribe, Pid}).

publish(Topic, Msg) ->
	Subscribers = mnesia:dirty_read(topic, Topic),
	[Sub ! {publish, Topic, Msg} || #topic{sub = Sub} <- Subscribers].

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.
%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({subscribe, Topic, Pid}, _From, State) ->
	Reply = mnesia:dirty_write(#topic{name = Topic, sub = Pid}),
    {reply, Reply, State};

handle_call(_Req, _From, State) ->
    {reply, {error, badreq}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({unsubscribe, Topic, Pid}, State) ->
	mnesia:dirty_delete_object(#topic{name = Topic, sub = Pid}),
    {noreply, State};

handle_cast({unsubscribe, Pid}, State) ->
	Subscribers = mnesia:dirty_match_object(#topic{sub=Pid}),
	[mnesia:dirty_delete(Sub) || Sub <- Subscribers],
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
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
    ok.
%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

