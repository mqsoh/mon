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
% This file was generated from the program.md in the root of this repository.
-module(mon_app).
-behavior(application).
-export([start/2, stop/1]).

start(_Start_type, _Start_args) ->
  <<Start applications required by the HTTP client.>>
  mon_sup:start_link().

stop(_State) ->
  ok.
```

All this module does is start the supervisor which is named, also by
convention, `mon_sup`.

After I've got the worker processes running, I'll [start applications required
by the HTTP client](#start-applications-required-by-the-http-client).



# The Supervisor

###### file:code/src/mon_sup.erl

```{name="file:code/src/mon_sup.erl"}
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
% This file was generated from the program.md in the root of this repository.
-module(mon).
-behavior(gen_server).
-export([
  <<mon exports>>,

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
    <<mon calls>>
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

<<mon functions>>
```

There's three code sections defined above. The `<<mon exports>>` section is
where exported functions are defined; the trailing comma means that any lines
I append to the section will end in a comma. The `<<mon calls>>` is inside
a case statement and is where I'll match calls from `gen_server:call`. Finally,
the `<<mon functions>>` section is where I'll define the mon API functions.

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

###### mon exports
```{.erlang name="mon exports"}
watch/1
```
###### mon functions
```{.erlang name="mon functions"}
watch(Url) ->
  gen_server:call(?MODULE, {watch, Url}).
```
###### mon calls
```{.erlang name="mon calls"}
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
```

This establishes a facet of the `worker` module. It needs a `start_link/2`
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
    <<worker calls>>
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
```

Check out that `init` function -- the `timer:apply_interval` is the heart beat
for this process. (It's cancelled in the `terminate` function.) The `state` for
this module is a map with the name of the running process, a reference to the
timer.

## The Heartbeat

If this were a real application, I wouldn't do that dumb heartbeat. What
happens if a response takes longer than the length of the heartbeat? Also, if
the target site goes down, I'd want to implement exponential back off. For now
it's this shit.


###### Start applications required by the HTTP client.
```{.erlang name="Start applications required by the HTTP client."}
ok = application:ensure_started(asn1),
ok = application:ensure_started(crypto),
ok = application:ensure_started(public_key),
ok = application:ensure_started(ssl),
ok = application:ensure_started(inets),
```

Now I can request a web page and store the status code.

###### worker calls
```{.erlang name="worker calls"}
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
```

`httpc:request` has three different return values. At a glance I couldn't
determine why it would return a 'request id', so I updated the status with
a representation of whatever it is.



# Worker Status

Now I'm going to rocks your socks off. With this literate program, I can always
append to a section. Any sections with the same name are concatenated. That
means that I can add features to both the `mon` and `worker` modules in the
same place.

The mon export.

```{.erlang name="mon exports"}
status/1
```

The mon API function.

```{.erlang name="mon functions"}
status(Url) ->
  gen_server:call(?MODULE, {status, Url}).
```

Handling the mon call.

```{.erlang name="mon calls"}
{status, Url} ->
  Name = list_to_atom(Url),
  {reply, gen_server:call(Name, status), State};
```

Returning the status from the worker.

```{.erlang name="worker calls"}
status ->
  #{last_status := Status} = State,
  {reply, Status, State};
```



# The Status of All Workers

The mon process state is a list of all the process names of the workers. What
I'd like to do is loop through them and print the process name and its current
status.

First export the API function.

```{.erlang name="mon exports"}
status/0
```

Then wrap the gen_server.

```{.erlang name="mon functions"}
status() ->
  gen_server:call(?MODULE, status).
```

And then loop through them and print.

```{.erlang name="mon calls"}
status ->
  lists:map(fun (Name) ->
              io:format("~s: ~s~n", [Name, gen_server:call(Name, status)])
            end,
            State),
  {reply, ok, State};
```

Now you can use `mon:status().` to get a list of the last status code for all
workers.



# Removing a Worker

You'll want to stop watching sites that you've added.

```{.erlang name="mon exports"}
rm/1
```

```{.erlang name="mon functions"}
rm(Url) ->
  gen_server:call(?MODULE, {rm, Url}).
```

```{.erlang name="mon calls"}
{rm, Url} ->
  Name = list_to_atom(Url),
  supervisor:terminate_child(mon_sup, Name),
  {reply, ok, lists:delete(Name, State)};
```



# A Sample Session

First you should build the Docker images with:

    ./build

Then you can run the shell. (The `build` and `shell` scripts are described in
the [development environment][].)

    $ ./shell
    Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

    Changed: ["development_environment.md","program.md"]
    Eshell V7.1  (abort with ^G)
    1> Compile: ./code/src/mon.erl
    Reloading: mon
    Compile: ./code/src/worker.erl
    Reloading: worker
    Compile: ./code/src/mon_sup.erl
    Reloading: mon_sup
    Compile: ./code/src/mon_app.erl
    Reloading: mon_app
    Site1 = "https://www.google.com", Site2 = "http://httpbin.org/status/418", Site3 = "This isn't a URL.".
    "This isn't a URL."
    2> application:start(mon).
    ok
    3> mon:watch(Site1), mon:watch(Site2), mon:watch(Site3).
    ok
    4> mon:status().
    This isn't a URL.: Waiting on first heartbeat.
    http://httpbin.org/status/418: Waiting on first heartbeat.
    https://www.google.com: Waiting on first heartbeat.
    ok
    5> % Waiting ten seconds.
    5> mon:status().
    This isn't a URL.: httpc error: no_scheme
    http://httpbin.org/status/418: 418 I'M A TEAPOT
    https://www.google.com: 200 OK
    ok
    6> mon:rm(Site3).
    ok
    7> mon:status().
    http://httpbin.org/status/418: 418 I'M A TEAPOT
    https://www.google.com: 200 OK
    ok
    8> mon:status(Site2).
    ["418",32,"I'M A TEAPOT"]
    9> application:stop(mon).

    =INFO REPORT==== 10-Dec-2015::05:51:29 ===
        application: mon
        exited: stopped
        type: temporary
    ok



# For the Future (But Probably Not)

I should be able to use names and configure parameters in the workers.
`mon:watch` should look more like

    mon:watch(google, "https://www.google.com/", #{frequency => 10 * 60 * 1000})

I should be able to check things other than the status code. This could be
different types of worker modules, like `worker_grep` that looks inside the
body of the page. Then the supervisor will watch processes of `mon`, `worker`,
and/or `worker_grep`.

Or maybe the request comparison is done with a callback. Then `mon:watch` would
look like

    mon:watch(google, "https://www.google.com/", #{frequency => 10 * 60 * 1000,
                                                   checker => {my_status_checkers, grep, []}})

With a module like

    -module(my_status_checkers).

    grep(<httpc:request response>) ->
      ...

Shouldn't I be able to *do* something with the status checks? Send an email or
something.

The sites I'm watching should be persisted somewhere and automatically loaded
when the mon application is started.

I should maintain a history with past events and the time they were returned.

In any case, I made a supervision tree and a dinky app, so mission
accomplished.



[development environment document]: development_environment.md
[The OTP design principles]: http://www.erlang.org/doc/design_principles/des_princ.html
[a list of the man pages]: http://www.erlang.org/doc/man_index.html
[erlang.org]: http://www.erlang.org/
[Modules]: http://www.erlang.org/doc/man_index.html
[`mon.app`]: http://www.erlang.org/doc/man/app.html
[the supervisor man page]: http://www.erlang.org/doc/man/supervisor.html
[development environment]: development_environment.md
