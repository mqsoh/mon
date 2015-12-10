% This file was generated from the program.md in the root of this repository.
-module(worker).
-behavior(gen_server).
-export([
  % Supervisor `start` function.
  start_link/2,

  % gen_server callbacks
  init/1,
  handle_call/3,
  terminate/2,

  % Unused gen_server callbacks.
  handle_cast/2,
  handle_info/2,
  code_change/3
]).

start_link(Name, Url) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Url], []).

init([Name, Url]) ->
  {ok, Timer} = timer:apply_interval(10000, gen_server, call, [Name, heartbeat]),
  {ok, #{name => Name,
         timer => Timer,
         url => Url,
         last_status => "Waiting on first heartbeat."}}.

handle_call(Request, _From, State) ->
  case Request of
    heartbeat ->
      #{url := Url} = State,
      Status = case httpc:request(Url) of
        {error, Reason} ->
          io_lib:format("httpc error: ~p", [Reason]);
    
        {ok, {{_Http_version, Status_code, Status_name}, _Headers, _Body}} ->
          io_lib:format("~B ~s", [Status_code, Status_name]);
    
        {ok, {Status_code, _Body}} ->
          integer_to_list(Status_code);
    
        {ok, Request_id} ->
          io_lib:format("Why did I get a request ID (~p)?", [Request_id])
      end,
      {noreply, maps:put(last_status, Status, State)};
    status ->
      #{last_status := Status} = State,
      {reply, Status, State};
    _ ->
      {noreply, State}
  end.

terminate(_Reason, #{timer := Timer}) ->
  timer:cancel(Timer).

% Unused gen_server callbacks.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

code_change(_Old_version, State, _Extra) ->
  {ok, State}.