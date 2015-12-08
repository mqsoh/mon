# About the Text

In this document I will periodically link to man pages. I reference them
locally with `erl -man <module>`, but they are also available online. I
recently figured out how to get [a list of the man pages][]. That's impressive
because the erlang.org site is confusing.

1.  Go to [erlang.org][].
1.  Click `Documentation` in the top nav.
1.  Click `Erlang/OTP Documentation`.

    Incidentally, in the description text for this link, there's another link
    to a different version: 'Searchable in the right column'. I don't ever use
    this because the search is in a frame and the main window's link doesn't
    update. If I want to link to it, I have to then jump through hoops.
1.  Just under the logo, there's a [Modules][] link. That's it!

When I create files from this literate document, I'm going to output them in
the `code` directory. This is the first time I'm doing that and the reason is
that the literate program should take priority. I don't want the literate
program mixed up with its output anymore.

Of course, I broke that rule with the [development environment document][], but
whatever.



# Mon

`Mon` will be a program that monitors the response codes of web sites. This is
a demo application to get me more familiar with supervision trees. The tree
will look like:

    Application (callback module is `mon_app`)
      v
    Supervisor (callback module is `mon_sup`)
      |
      |---------.-----------.-----------.------.
      v         v           v           v      v
    Mon API    Worker 1    Worker 2    ...    Worker N

The mon API and worker nodes will be `gen_server`s because I don't know what
else they should be. [The OTP design principles][] lists the standard OTP
behaviors; none seem appropriate. The workers will have unimplemented functions
but this also seems typical in Erlang. I think it's okay because there's not
a lot of ceremony around these functions (they usually look like `whatever(_)
-> ok.`. Anyway...



# The Application

When you run `application:start(mon).`, the `application` module searches the
code path for a `[mon.app][]` file. The `mod` specified therein is called 'the
application callback module'.

(When working with `rebar` or other Erlang build tools, an `.app.src` is put in
the `src` directory and copied into `ebin` when the code is compiled. I don't
think it's a big deal to check in a `.app` into the ebin directory under source
control.)

###### file:code/ebin/mon.app

```{name="file:code/ebin/mon.app"}
{application, mon,
  [{mod, {mon_app, []}}]}.
```

The application module starts a master process and runs the `start/2` function
in the callback module.

###### file:code/src/mon_app.erl

```{name="file:code/src/mon_app.erl"}
-module(mon_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Start_type, _Start_args) ->
  io:format("mon: Starting app.~n"),
  mon_sup:start_link().

stop(_State) ->
  io:format("mon: Stopping app.~n"),
  ok.
```

All this module does is start the supervisor which is named, also by
convention, `mon_sup`.



# The Supervisor

###### file:code/src/mon_sup.erl

```{name="file:code/src/mon_sup.erl"}
-module(mon_sup).
-behavior(supervisor).
-export([
  start_link/0,
  init/1
]).

start_link() ->
  io:format("mon: Supervisor starting link.~n"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("mon: Supervisor init.~n"),
  {ok, {
      <<mon restart strategy>>
      ,
      <<mon child spec>>
  }}.
```

This module is a 'supervisor callback module', which only needs to define an
`init/1` function. The `start_link/0` function is not a part of the supervisor
behavior and seems like a pointless abstraction to me. However, it seems to be
a convention and I will follow it. (`mon_app:start/2` could just run
`supervisor:start_link({local, mon_sup}, mon_sup, [])`. Perhaps the usage of
`?MODULE` makes it easier to refactor...)

When this function is called it

> Creates a supervisor process as part of a supervision tree. The function
> will, among other things, ensure that the supervisor is linked to the calling
> process (its supervisor).

In this case, the application process is the calling process.

When you're wondering about what functions a callback module should support,
you can check the bottom of the man page for that module under `CALLBACK
FUNCTIONS`. Here's [the supervisor man page][].

## Restart Strategy and Child Spec

The `init` function returns the restart strategy and child spec. This is the
restart strategy.

```{name="mon restart strategy"}
#{strategy => one_for_one,
  intensity => 10,
  period => 1}
```

The `one_for_one` strategy means that if a child process dies, only that one
process is restarted. (The `one_for_all` strategy will kill and restart all the
children.) The intensity and period of restarts I selected is random. It
instructs OTP to shut down the children and their supervisor if more than ten
restarts happen in one second.

The following is the child spec. The `id` is used internally by the supervisor.
The `start` value is a `module-function-arguments` tuple. You can specify
a `type`, which can be `worker` or `supervisor`; I don't remember the details,
but it affects how the process is handled when it's killed. The `mon` process
will be a `gen_server`, so I'll use the default value of `worker`.

```{name="mon child spec"}
[#{id => mon,
   start => {mon, start_link, []}}]
```



# The Mon Process

Our process has been started by the supervisor. In the following block of code,
only the `start_link/0`, `init/1` and `handle_call/3` are used.

###### file:code/src/mon.erl

```{.erlang name="file:code/src/mon.erl"}
-module(mon).
-behavior(gen_server).
-export([
  <<mon api>>,

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
  io:format("mon: mon:start_link()~n"),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  io:format("mon: mon:init([])~n"),
  {ok, []}.

handle_call(Request, From, State) ->
  io:format("mon: mon:handle_call(~p, ~p, ~p)~n", [Request, From, State]),
  case Request of
    <<handle mon calls>>
    _ ->
      {noreply, State}
  end.

% Unused gen_server callbacks.

handle_cast(Request, State) ->
  io:format("mon: mon:handle_cast(~p, ~p)~n", [Request, State]),
  {noreply, State}.

handle_info(Info, State) ->
  io:format("mon: mon:handle_info(~p, ~p)~n", [Info, State]),
  {noreply, State}.

terminate(Reason, State) ->
  io:format("mon: mon:terminate(~p, ~p)~n", [Reason, State]),
  return_value_ignored.

code_change(Old_version, State, Extra) ->
  io:format("mon: mon:code_change(~p, ~p, ~p)~n", [Old_version, State, Extra]),
  {ok, State}.

<<mon functions>>
```

There's three code sections defined above. The `<<mon api>>` section is where
exported functions are defined; the trailing comma means that any lines
I append to the section will end in a comma. The `<<handle mon calls>>` is
inside a case statement and is where I'll match calls from `gen_server:call`.
Finally, the `<<mon functions>>` section is where I'll define the mon API
functions.

This is the real power of a literate program. The compiler needs code in
various places inside the file, but having related code grouped together helps
humans understand.

## Starting Workers

This is my first usage of the three code sections defined in the `mon` module.
They will be used to add behavior to the `mon` process. The exported functions
will all be similar in that they will simply pass on arguments to
`gen_server:call`. The handle sections are embedded inside that `case`
statement above. The only bit of context I need to remember is that the `State`
is a list of the names of started `worker` processes.

###### mon api
```{.erlang name="mon api"}
watch/1
```
###### mon functions
```{.erlang name="mon functions"}
watch(Url) ->
  gen_server:call(?MODULE, {watch, Url}).
```
###### handle mon calls
```{.erlang name="handle mon calls"}
{watch, Url} ->
  Name = list_to_atom(Url),
  Ret = supervisor:start_child(mon_sup, #{id => Name,
                                          start => {worker, start_link, [Name]}}),
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
```

This establishes a facet of the `worker` module. It needs at `start_link/1`
function that uses the given name when registering with the supervisor. In the
`mon` module, I was able to use the `?MODULE` macro as the name of the process.
That was okay because it was a globally unique name. I'll have many worker
processes, each with their own name.



# The Worker Processes

Workers will also be a `gen_server` because it's the most generic OTP behavior.
I'll have the same unused functions as in `mon`. That's kind of not cool, so
I'm not sure if it's correct. I know Elixir added a couple even more generic
behaviors. I think they call them 'events' and 'tasks' or something. I wonder
if I could add something like that...

One thing I'll do differently is that, since the `mon` module is the API
I won't bother writing the same sort of API for the worker processes. I'll just
write `gen_server:call`s in the `mon` module so that I'll only need to augment
`handle_call` in this module.

###### file:code/src/worker.erl
```{.erlang name="file:code/src/worker.erl"}
-module(worker).
-behavior(gen_server).
-export([
  % Supervisor `start` function.
  start_link/1,

  % gen_server callbacks
  init/1,
  handle_call/3,
  terminate/2,

  % Unused gen_server callbacks.
  handle_cast/2,
  handle_info/2,
  code_change/3
]).

start_link(Name) ->
  io:format("mon: worker:start_link(~p)~n", [Name]),
  gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init([Name]) ->
  io:format("mon: worker:init(~p)~n", [[Name]]),
  {ok, Timer} = timer:apply_interval(5000, gen_server, call, [Name, heartbeat]),
  {ok, #{name => Name, timer => Timer}}.

handle_call(Request, From, State) ->
  io:format("mon: worker:handle_call(~p, ~p, ~p)~n", [Request, From, State]),
  case Request of
    <<worker calls>>
    _ ->
      {noreply, State}
  end.

terminate(Reason, #{timer := Timer} = State) ->
  io:format("mon: worker:terminate(~p, ~p)~n", [Reason, State]),
  timer:cancel(Timer).

% Unused gen_server callbacks.

handle_cast(Request, State) ->
  io:format("mon: worker:handle_cast(~p, ~p)~n", [Request, State]),
  {noreply, State}.

handle_info(Info, State) ->
  io:format("mon: worker:handle_info(~p, ~p)~n", [Info, State]),
  {noreply, State}.

code_change(Old_version, State, Extra) ->
  io:format("mon: worker:code_change(~p, ~p, ~p)~n", [Old_version, State, Extra]),
  {ok, State}.
```

Check out that `init` function -- the `timer:apply_interval` is the heart beat
for this process. (It's cancelled in the `terminate` function.) The `state` for
this module is a map with the name of the running process, a reference to the
timer.

**TODO**: The state will need a list of statuses, too.

## Test worker function

```{.erlang name="worker calls"}
status ->
  {reply, "I'm okay. How are you?", State};
```




[development environment document]: development_environment.md
[The OTP design principles]: http://www.erlang.org/doc/design_principles/des_princ.html
[a list of the man pages]: http://www.erlang.org/doc/man_index.html
[erlang.org]: http://www.erlang.org/
[Modules]: http://www.erlang.org/doc/man_index.html
[`mon.app`]: http://www.erlang.org/doc/man/app.html
[the supervisor man page]: http://www.erlang.org/doc/man/supervisor.html
