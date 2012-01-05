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

-module(emqtt_types).

-include("emqtt.hrl").

-ifdef(use_specs).

-export_type([maybe/1, info/0, infos/0, info_key/0, info_keys/0,
              ctag/0, listener/0,
              connection/0, mqtt_packet/0, 
              username/0, password/0, ok/1, error/1,
              ok_or_error/1, ok_or_error2/2, ok_pid_or_error/0, channel_exit/0,
              connection_exit/0]).

-type(channel_exit() :: no_return()).
-type(connection_exit() :: no_return()).

-type(maybe(T) :: T | 'none').
-type(ctag() :: binary()).

%% TODO: make this more precise by tying specific class_ids to
%% specific properties
-type(info_key() :: atom()).
-type(info_keys() :: [info_key()]).

-type(info() :: {info_key(), any()}).
-type(infos() :: [info()]).

-type(listener() ::
        #listener{node     :: node(),
                  host     :: emqtt_networking:hostname(),
                  port     :: emqtt_networking:ip_port()}).
-type(mqtt_packet() ::
        #mqtt_packet{id			  :: integer(),
                  type	          :: integer(),
                  dup	          :: boolean(),
                  qos			  :: integer(),
                  retain	      :: boolean(),
                  arg			  :: any()}).

-type(connection() :: pid()).

-type(username() :: binary()).
-type(password() :: binary()).

-type(ok(A) :: {'ok', A}).
-type(error(A) :: {'error', A}).
-type(ok_or_error(A) :: 'ok' | error(A)).
-type(ok_or_error2(A, B) :: ok(A) | error(B)).
-type(ok_pid_or_error() :: ok_or_error2(pid(), any())).

-endif. % use_specs
