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
      v
    Supervisor (callback module is `mon`)
      |
      |-----------.-----------.------.
      v           v           v      v
    Worker 1    Worker 2    ...    Worker N

The worker nodes will be `gen_server`s because I don't know what else they
should be. [The OTP design principles][] lists the standard OTP behaviors; none
seem appropriate. The workers will have basically unimplemented functions. This
also seems typical in Erlang. I think it's okay because there's not a lot of
ceremony around these functions. Anyway...



# The Application

When you run `application:start(mon).`, the `application` module searches the
code path for a [`mon.app`][] file. The `mod` specified therein is called 'the
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
  supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  supervisor:start_child(?MODULE, []).

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

The `start_link` function ends by starting its own child.

## Restart Strategy and Child Spec

The `init` function returns the restart strategy and child spec. This is the
restart strategy.

```{name="mon restart strategy"}
#{strategy => simple_one_for_one,
  intensity => 10,
  period => 1}
```

The `simple_one_for_one` strategy refers to a simplified version of
`one_for_one`. This is used when you only need one child process type. The
intensity and period of restarts I selected is random. It instructs OTP to shut
down the children and their supervisor if more than ten restarts happen in one
second.

The following is the child spec. The `id` is used internally by the supervisor.
The `start` value is a `module-function-arguments` tuple. The type can be
`worker` or `supervisor`; I don't remember the details, but it affects how the
process is handled when it's killed.

```{name="mon child spec"}
[#{id => mon,
   start => {mon, start_link, []},
   type => supervisor}]
```



# Our Process

Our process is the second supervisor in the supervisor tree. It's very similar
to `mon_sup` -- so similar that I'm wondering if I need both. When the first
argument to `start_link` is `{local, whatever}` it means that the process is
registered under that name. (You can send messages to it by name instead of a
PID.)

###### file:code/src/mon.erl
```{.erlang name="file:code/src/mon.erl"}
-module(mon).
-behavior(supervisor).
-export([
  <<mon api>>,
  start_link/0,
  init/1
]).

start_link() ->
  io:format("mon: Mon process started.~n"),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  io:format("mon: Mon process inited.~n"),
  {ok, {
    #{strategy => simple_one_for_one,
      intensity => 10,
      period => 1}
    ,
    [#{id => worker,
       start => {worker, start_link, []}}]
  }}.

<<mon functions>>

<<mon receive loop>>
```

The restart strategy is the same as in `mon_sup` because I can't make an
informed decision about that at the moment. We only have one type of child
process, so `simple_one_for_one` is appropriate again.

This is the process I want to send messages to, so the end of `start_link`
starts the receive `loop`.

**NOTE**: There's three sections above: `mon api`, `mon functions`, and `mon
receive loop`. These are placeholders for code I will be writing later in the
document.

## Messages to the Mon Process

I'll provide a public API to the mon process, but those functions will be
sending messages to the registered `mon` process. This is typical in Erlang.
Originally, I thought it looked like a process was sending a message to itself,
but that's wrong.

If I have a function that looks like `foobar() -> ?MODULE ! {self(), foobar}.`
and I call it from the shell with `mymodule:foobar().`, then `self()` is not
the process registered under the `mymodule` name. It's the shell PID. It's
strange at first, but I've seen this done in Joe Armstrong's code and has the
benefit of simple usage. You can say:

> Start the mon application. The API exposes the following functions: `mon:ls`,
> `mon:watch`, etc.

The user doesn't need to know anything about the supervision tree.

Anyway, here's that standard receive loop.

###### mon receive loop
```{.erlang name="mon receive loop"}
loop() ->
  io:format("mon: Receive loop iteration.  FART ~p~n", [self()]),
  receive
    <<incoming mon message>>
    Any ->
      io:format("mon: An unhandled message was recieved: ~p~n", [Any])
  end.
```

## List

```{name="mon api"}
list/0
```

```{name="incoming mon message"}
{Pid, list} ->
  io:format("mon: I see that you want a list, but I'm not doing anything yet.~n");
```

```{name="mon functions"}
list() ->
  ?MODULE ! {self(), list}.
```






[development environment document]: development_environment.md
[The OTP design principles]: http://www.erlang.org/doc/design_principles/des_princ.html
[a list of the man pages]: http://www.erlang.org/doc/man_index.html
[erlang.org]: http://www.erlang.org/
[Modules]: http://www.erlang.org/doc/man_index.html
[`mon.app`]: http://www.erlang.org/doc/man/app.html
[the supervisor man page]: http://www.erlang.org/doc/man/supervisor.html
