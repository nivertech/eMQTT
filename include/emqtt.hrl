%copy from mqtt4erl

-define(MQTT_PORT, 1883).

-define(MQTT_PROTOCAL_NAME, "MQIsdp").
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

-record(mqtt, {
  id,
  type,
  dup = 0,
  qos = 0,
  retain = 0,
  args
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

