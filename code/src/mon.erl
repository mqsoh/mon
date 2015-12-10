% This file was generated from the program.md in the root of this repository.
-module(mon).
-behavior(gen_server).
-export([
  watch/1,
  status/1,
  status/0,
  rm/1,

  % Supervisor `start` function.
  start_link/0,

  % gen_server callbacks
  init/1,
  handle_call/3,

  % Unused gen_server callbacks.
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  {ok, []}.

handle_call(Request, _From, State) ->
  case Request of
    {watch, Url} ->
      Name = list_to_atom(Url),
      Ret = supervisor:start_child(mon_sup, #{id => Name,
                                              start => {worker, start_link, [Name, Url]}}),
      case Ret of
        {ok, _} ->
          {reply, ok, [Name | State]};
        {ok, _, _} ->
          {reply, ok, [Name | State]};
        {error, {already_started, _}} ->
          {reply, already_started, State};
        {error, Reason} ->
          {reply, Reason, State}
      end;
    {status, Url} ->
      Name = list_to_atom(Url),
      {reply, gen_server:call(Name, status), State};
    status ->
      lists:map(fun (Name) ->
                  io:format("~s: ~s~n", [Name, gen_server:call(Name, status)])
                end,
                State),
      {reply, ok, State};
    {rm, Url} ->
      Name = list_to_atom(Url),
      supervisor:terminate_child(mon_sup, Name),
      {reply, ok, lists:delete(Name, State)};
    _ ->
      {noreply, State}
  end.

% Unused gen_server callbacks.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  return_value_ignored.

code_change(_Old_version, State, _Extra) ->
  {ok, State}.

watch(Url) ->
  gen_server:call(?MODULE, {watch, Url}).
status(Url) ->
  gen_server:call(?MODULE, {status, Url}).
status() ->
  gen_server:call(?MODULE, status).
rm(Url) ->
  gen_server:call(?MODULE, {rm, Url}).