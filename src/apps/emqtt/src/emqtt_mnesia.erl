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

-module(emqtt_mnesia).

-export([ensure_mnesia_dir/0, dir/0, status/0, init/0, is_db_empty/0,
         init_db/0,
         is_clustered/0, running_clustered_nodes/0, all_clustered_nodes/0,
         empty_ram_only_tables/0, wait_for_tables/1,
         create_cluster_nodes_config/1, read_cluster_nodes_config/0,
         record_running_nodes/0, read_previously_running_nodes/0,
         delete_previously_running_nodes/0, running_nodes_filename/0,
         on_node_down/1, on_node_up/1]).

-export([table_names/0]).

%% create_tables/0 exported for helping embed RabbitMQ in or alongside
%% other mnesia-using Erlang applications, such as ejabberd
-export([create_tables/0]).

-include("emqtt.hrl").

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-export_type([node_type/0]).

-type(node_type() :: disc_only | disc | ram | unknown).
-spec(status/0 :: () -> [{'nodes', [{node_type(), [node()]}]} |
                         {'running_nodes', [node()]}]).
-spec(dir/0 :: () -> file:filename()).
-spec(ensure_mnesia_dir/0 :: () -> 'ok').
-spec(init/0 :: () -> 'ok').
-spec(init_db/0 :: () -> 'ok').
-spec(is_db_empty/0 :: () -> boolean()).
-spec(running_clustered_nodes/0 :: () -> [node()]).
-spec(all_clustered_nodes/0 :: () -> [node()]).
-spec(empty_ram_only_tables/0 :: () -> 'ok').
-spec(create_tables/0 :: () -> 'ok').
-spec(wait_for_tables/1 :: ([atom()]) -> 'ok').
-spec(create_cluster_nodes_config/1 :: ([node()]) ->  'ok').
-spec(read_cluster_nodes_config/0 :: () ->  [node()]).
-spec(record_running_nodes/0 :: () ->  'ok').
-spec(read_previously_running_nodes/0 :: () ->  [node()]).
-spec(delete_previously_running_nodes/0 :: () ->  'ok').
-spec(running_nodes_filename/0 :: () -> file:filename()).
-spec(on_node_up/1 :: (node()) -> 'ok').
-spec(on_node_down/1 :: (node()) -> 'ok').

-spec(table_names/0 :: () -> [atom()]).

-endif.

%%----------------------------------------------------------------------------

status() ->
    [{nodes, case mnesia:system_info(is_running) of
                 yes -> [{Key, Nodes} ||
                            {Key, CopyType} <- [{disc_only, disc_only_copies},
                                                {disc,      disc_copies},
                                                {ram,       ram_copies}],
                            begin
                                Nodes = nodes_of_type(CopyType),
                                Nodes =/= []
                            end];
                 no -> case all_clustered_nodes() of
                           [] -> [];
                           Nodes -> [{unknown, Nodes}]
                       end;
                 Reason when Reason =:= starting; Reason =:= stopping ->
                     exit({emqtt_busy, try_again_later})
             end},
     {running_nodes, running_clustered_nodes()}].

init() ->
    ensure_mnesia_running(),
    ensure_mnesia_dir(),
    case mnesia:system_info(extra_db_nodes) of
    [] -> %master node
		ok = init_db();
    _ -> %slave node
        ok
    end,
    %% We intuitively expect the global name server to be synced when
    %% Mnesia is up. In fact that's not guaranteed to be the case - let's
    %% make it so.
    ok = global:sync(),
    ok.

is_db_empty() ->
    lists:all(fun (Tab) -> mnesia:dirty_first(Tab) == '$end_of_table' end,
              table_names()).
%% return node to its virgin state, where it is not member of any
%% cluster, has no cluster configuration, no local database, and no
%% persisted messages
is_clustered() ->
    RunningNodes = running_clustered_nodes(),
    [node()] /= RunningNodes andalso [] /= RunningNodes.

all_clustered_nodes() ->
    mnesia:system_info(db_nodes).

running_clustered_nodes() ->
    mnesia:system_info(running_db_nodes).

empty_ram_only_tables() ->
    Node = node(),
    lists:foreach(
      fun (TabName) ->
              case lists:member(Node, mnesia:table_info(TabName, ram_copies)) of
                  true  -> {atomic, ok} = mnesia:clear_table(TabName);
                  false -> ok
              end
      end, table_names()),
    ok.

%%--------------------------------------------------------------------

nodes_of_type(Type) ->
    %% This function should return the nodes of a certain type (ram,
    %% disc or disc_only) in the current cluster.  The type of nodes
    %% is determined when the cluster is initially configured.
    mnesia:table_info(schema, Type).

%% The tables aren't supposed to be on disk on a ram node
table_definitions() ->
    [{emqtt_listener,
      [{record_name, listener},
       {attributes, record_info(fields, listener)},
       {type, bag},
       {match, #listener{_='_'}}]}
	] ++ gm:table_definitions().

table_names() ->
    [Tab || {Tab, _} <- table_definitions()].

dir() -> mnesia:system_info(directory).

ensure_mnesia_dir() ->
    MnesiaDir = dir() ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
        ok ->
            ok
    end.

ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes ->
            ok;
        starting ->
            wait_for(mnesia_running),
            ensure_mnesia_running();
        Reason when Reason =:= no; Reason =:= stopping ->
            throw({error, mnesia_not_running})
    end.

ensure_mnesia_not_running() ->
    case mnesia:system_info(is_running) of
        no ->
            ok;
        stopping ->
            wait_for(mnesia_not_running),
            ensure_mnesia_not_running();
        Reason when Reason =:= yes; Reason =:= starting ->
            throw({error, mnesia_unexpectedly_running})
    end.



%% The cluster node config file contains some or all of the disk nodes
%% that are members of the cluster this node is / should be a part of.
%%
%% If the file is absent, the list is empty, or only contains the
%% current node, then the current node is a standalone (disk)
%% node. Otherwise it is a node that is part of a cluster as either a
%% disk node, if it appears in the cluster node config, or ram node if
%% it doesn't.

cluster_nodes_config_filename() ->
    dir() ++ "/cluster_nodes.config".

create_cluster_nodes_config(ClusterNodes) ->
    FileName = cluster_nodes_config_filename(),
    case emqtt_file:write_term_file(FileName, [ClusterNodes]) of
        ok -> ok;
        {error, Reason} ->
            throw({error, {cannot_create_cluster_nodes_config,
                           FileName, Reason}})
    end.

read_cluster_nodes_config() ->
    FileName = cluster_nodes_config_filename(),
    case emqtt_file:read_term_file(FileName) of
        {ok, [ClusterNodes]} -> ClusterNodes;
        {error, enoent} ->
            {ok, ClusterNodes} = application:get_env(emqtt, cluster_nodes),
            ClusterNodes;
        {error, Reason} ->
            throw({error, {cannot_read_cluster_nodes_config,
                           FileName, Reason}})
    end.

running_nodes_filename() ->
    filename:join(dir(), "nodes_running_at_shutdown").

record_running_nodes() ->
    FileName = running_nodes_filename(),
    Nodes = running_clustered_nodes() -- [node()],
    %% Don't check the result: we're shutting down anyway and this is
    %% a best-effort-basis.
    emqtt_file:write_term_file(FileName, [Nodes]),
    ok.

read_previously_running_nodes() ->
    FileName = running_nodes_filename(),
    case emqtt_file:read_term_file(FileName) of
        {ok, [Nodes]}   -> Nodes;
        {error, enoent} -> [];
        {error, Reason} -> throw({error, {cannot_read_previous_nodes_file,
                                          FileName, Reason}})
    end.

delete_previously_running_nodes() ->
    FileName = running_nodes_filename(),
    case file:delete(FileName) of
        ok              -> ok;
        {error, enoent} -> ok;
        {error, Reason} -> throw({error, {cannot_delete_previous_nodes_file,
                                          FileName, Reason}})
    end.

%% Take a cluster node config and create the right kind of node - a
%% standalone disk node, or disk or ram node connected to the
%% specified cluster nodes.  If Force is false, don't allow
%% connections to offline nodes.
init_db() ->
	ok = create_tables().

create_tables() ->
    lists:foreach(fun ({Tab, TabDef}) ->
		io:format("~ncreating table ~p", [Tab]),
                          TabDef1 = proplists:delete(match, TabDef),
                          case mnesia:create_table(Tab, TabDef1) of
                              {atomic, ok} -> ok;
                              {aborted, Reason} ->
                                  throw({error, {table_creation_failed,
                                                 Tab, TabDef1, Reason}})
                          end
                  end,
                  table_definitions()),
    ok.

wait_for_tables() -> wait_for_tables(table_names()).

wait_for_tables(TableNames) ->
    case mnesia:wait_for_tables(TableNames, 30000) of
        ok ->
            ok;
        {timeout, BadTabs} ->
            throw({error, {timeout_waiting_for_tables, BadTabs}});
        {error, Reason} ->
            throw({error, {failed_waiting_for_tables, Reason}})
    end.


wait_for(Condition) ->
    error_logger:info_msg("Waiting for ~p...~n", [Condition]),
    timer:sleep(1000).

on_node_up(Node) ->
    case is_only_disc_node(Node, true) of
        true  -> emqtt_log:info("cluster contains disc nodes again~n");
        false -> ok
    end.

on_node_down(Node) ->
    case is_only_disc_node(Node, true) of
        true  -> emqtt_log:info("only running disc node went down~n");
        false -> ok
    end.

is_only_disc_node(Node, _MnesiaRunning = true) ->
    RunningSet = sets:from_list(running_clustered_nodes()),
    DiscSet = sets:from_list(nodes_of_type(disc_copies)),
    [Node] =:= sets:to_list(sets:intersection(RunningSet, DiscSet));
is_only_disc_node(Node, false) ->
    start_mnesia(),
    Res = is_only_disc_node(Node, true),
    stop_mnesia(),
    Res.

start_mnesia() ->
    emqtt_misc:ensure_ok(mnesia:start(), cannot_start_mnesia),
    ensure_mnesia_running().

stop_mnesia() ->
    stopped = mnesia:stop(),
    ensure_mnesia_not_running().
