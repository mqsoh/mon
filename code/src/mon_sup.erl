% This file was generated from the program.md in the root of this repository.
-module(mon_sup).
-behavior(supervisor).
-export([
  start_link/0,
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {
      #{strategy => one_for_one,
        intensity => 10,
        period => 1}
      ,
      [#{id => mon,
         start => {mon, start_link, []}}]
  }}.