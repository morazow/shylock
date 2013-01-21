#!/bin/sh

rel/Shylock/bin/Shylock stop
rm -rf rel/Shylock

git pull origin master
./rebar get-deps
./rebar compile

./rebar generate
rel/Shylock/bin/Shylock console
