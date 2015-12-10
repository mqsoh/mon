% This file was generated from the program.md in the root of this repository.
-module(mon_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Start_type, _Start_args) ->
  ok = application:ensure_started(asn1),
  ok = application:ensure_started(crypto),
  ok = application:ensure_started(public_key),
  ok = application:ensure_started(ssl),
  ok = application:ensure_started(inets),
  mon_sup:start_link().

stop(_State) ->
  ok.