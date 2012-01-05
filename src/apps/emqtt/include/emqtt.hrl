%copy from mqtt4erl

-define(MQTT_PORT, 1883).

-define(MQTT_PROTOCOL_NAME, "MQIsdp").
-define(MQTT_PROTOCOL_VERSION, 3).

-define(UNUSED, 0).

-define(DEFAULT_KEEPALIVE, 120).
-define(DEFAULT_RETRY, 120).
-define(DEFAULT_CONNECT_TIMEOUT, 5).

-define(CONNECT, 1).
-define(CONNACK, 2).
-define(PUBLISH, 3).
-define(PUBACK, 4).
-define(PUBREC, 5).
-define(PUBREL, 6).
-define(PUBCOMP, 7).
-define(SUBSCRIBE, 8).
-define(SUBACK, 9).
-define(UNSUBSCRIBE, 10).
-define(UNSUBACK, 11).
-define(PINGREQ, 12).
-define(PINGRESP, 13).
-define(DISCONNECT, 14).

-record(ssl_socket, {tcp, ssl}).

-record(listener, {node, protocol, host, ip_address, port}).

-record(mqtt_client, {
  socket,
  id_pid,
  owner_pid,
  ping_timer,
  retry_timer,
  pong_timer,
  connect_options
}).

-record(connect_options, {
  protocol_name = ?MQTT_PROTOCOL_NAME,
  protocol_version = ?MQTT_PROTOCOL_VERSION,
  client_id = mqtt_client:default_client_id(),
  clean_start = true,
  will,
  keepalive = ?DEFAULT_KEEPALIVE,
  retry = ?DEFAULT_RETRY,
  connect_timeout = ?DEFAULT_CONNECT_TIMEOUT
}).

-record(mqtt_packet, {
  id,
  type,
  dup = 0,
  qos = 0,
  retain = 0,
  arg
}).

-record(sub, {
  topic,
  qos = 0
}).

-record(publish_options, {
  qos = 0,
  retain = 0
}).

-record(will, {
  topic,
  message,
  publish_options = #publish_options{}
}).


-define(COPYRIGHT_MESSAGE, "Copyright (C) 2007-2011 VMware, Inc.").
-define(INFORMATION_MESSAGE, "Licensed under the MPL.  See http://www.rabbitmq.com/").
-define(PROTOCOL_VERSION, "AMQP 0-9-1 / 0-9 / 0-8").
-define(ERTS_MINIMUM, "5.6.3").

-define(MAX_WAIT, 16#ffffffff).

-define(HIBERNATE_AFTER_MIN,        1000).
-define(DESIRED_HIBERNATE,         10000).

-define(ROUTING_HEADERS, [<<"CC">>, <<"BCC">>]).
-define(DELETED_HEADER, <<"BCC">>).

-ifdef(debug).
-define(LOGDEBUG0(F), rabbit_log:debug(F)).
-define(LOGDEBUG(F,A), rabbit_log:debug(F,A)).
-define(LOGMESSAGE(D,C,M,Co), rabbit_log:message(D,C,M,Co)).
-else.
-define(LOGDEBUG0(F), ok).
-define(LOGDEBUG(F,A), ok).
-define(LOGMESSAGE(D,C,M,Co), ok).
-endif.

-define(LOG(Msg), io:format("{~p:~p ~p}: ~p~n", [?MODULE, ?LINE, self(), Msg])).


