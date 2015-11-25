# About the Text

In this document I will link periodically to man pages. I reference them
locally with `erl -man <module>`, but they are also available online. I
recently figured out how to get [a list of the man pages][]. The documentation
is confusing; I wasn't able to figure out how to get to it until recently.

1. Go to [erlang.org][].
1. Click `Documentation` in the top nav.
1.  Click `Erlang/OTP Documentation`.

    Incidentally, the description text mentions 'Searchable in the right
    column' which links to documentation that does have a search feature. The
    main link, which this text is describing doesn't -- weird!.
1. Just under the logo, there's a [Modules][] link. That's it!

When I create files from this literate document, I'm going to output them in
the `code` directory. This is the first time I'm doing that and the reason is
that the literate program should take priority. I don't want the literate
program mixed up with its output anymore.



# Mon

`Mon` will be a program that monitors the response codes of web sites. The
top-level module, `mon`, will have CRUD for managing the list of web sites.
When a web site is added, a process will be spawned that periodically checks
the HTTP response code. I'll call them `watchers`. The mon module will be a
supervisor for these processes.

I'm going to start with the CRUD without actually spawning any processes.



# The application.

When you run `application:start(mon).`, the `application` module searches the
code path for a [`mon.app`][] file. The `mod` specified therein is the application
callback module.

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
  mon_sup:start_link().

stop(_State) ->
  ok.
```

All this module does is start the supervisor which is named, also by
convention, `mon_sup`.

###### file:code/src/mon_sup.erl
```{name="file:code/src/mon_sup.erl"}
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

This module is a supervisor callback module, which only needs to define an
`init/1` function. The `start_link/0` function seems like a pointless
abstraction to me, however, it seems to be a convention. (`mon_app:start/2`
could just run `supervisor:start_link({local, mon_sup}, mon_sup, [])`. Perhaps
the usage of `?MODULE` makes it easier to refactor...)

When this function is called it

> Creates a supervisor process as part of a supervision tree. The function
> will, among other things, ensure that the supervisor is linked to the calling
> process (its supervisor).

In this case, the calling process is the application's master process.

When you're wondering about what functions a callback module should support,
you can check the bottom of the man page for that module under `CALLBACK
FUNCTIONS`. Here's [the supervisor man page][].

# Mon Restart Strategy and Child Spec

This is what the supervision tree should look like:

    (application (mon_app)) -> (supervisor (mon_sup)) -> (mon)

The `init` function returns the restart strategy and child spec. My experience
with Erlang is limited but this is the most complicated data structure I've had
to work with.

## Restart Strategy

```{name="mon restart strategy"}
#{strategy => simple_one_for_one,
  intensity => 10,
  period => 1}
```

The `simple_one_for_one` strategy refers to a simplified version of
`one_for_one`. This is used when you only need one child process type. The
intensity and period of restarts is random. It says that Erlang should shut
down the children and their supervisor if more than ten restarts happen in one
second.

```{name="mon child spec"}
[#{id => mon,
   start => {mon, start, []},
   type => supervisor}]
```




[a list of the man pages]: http://www.erlang.org/doc/man_index.html
[erlang.org]: http://www.erlang.org/
[Modules]: http://www.erlang.org/doc/man_index.html
[`mon.app`]: http://www.erlang.org/doc/man/app.html
[the supervisor man page]: http://www.erlang.org/doc/man/supervisor.html
