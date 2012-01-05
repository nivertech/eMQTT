# eMQTT

Erlang MQTT Server is based on RabbitMQ Network Framework.

# Build

NOTICE: should install rebar first

git clone https://github.com/basho/rebar
cd rebar
./bootstrap
cp rebar .

make 

# Run

cd rel

../rebar generate

cd emqtt

./bin/emqtt [console | start | stop]

# Credits

The first release is based on rqbbitmq.

www.rabbitmq.com

