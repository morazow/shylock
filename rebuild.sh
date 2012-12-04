#!/bin/sh

rel/paxy/bin/paxy stop
rm -rf rel/paxy

git pull origin Paxyv3
./rebar get-deps
./rebar compile

./rebar generate
rel/paxy/bin/paxy console
