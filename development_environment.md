# Development Environment

My goal is to create a Dockerfile that I can use for development. It will use
my literate program [knot][] to generate source code from my literate program,
compile it, and reload it into a running Erlang shell.

There's an official Erlang image. Since I want to use it in a development
environment, it'd be nice if there were an easy way to add the man pages for
the installed version. However, `/usr/lib/erlang/lib/misc/format_man_pages` has
dependencies that I don't understand.

I've built [Erlang from source][] before and there's some specific requirements
to build the documentation that [the official Dockerfile][] doesn't account
for. (Not that it should -- I wouldn't want them to bundle man pages. I just
wish it were easy to add them.)

```{.Dockerfile name="file:Dockerfile"}
FROM erlang:18
```

# Knot

[The `code` man page][] explains how code paths are searched. In the image,
Erlang is installed in /usr/lib/erlang, so I can put knot in
`/usr/local/lib/erlang/lib/knot` and the compiled `beam` files in `ebin` will
be available at run time.

```{.Dockerfile name="file:Dockerfile"}
RUN git clone https://github.com/mqsoh/knot.git /usr/local/lib/erlang/lib/knot
RUN cd /usr/local/lib/erlang/lib/knot && erl -noshell -s make all -s init stop
```

# Starting the Shell

I want to just run the container and have it provide a shell that is already
recompiling my files and reloading the module into the shell.

When I wrote the knot function that watches for file changes and recompiles the
literate program, I wrote something that polls the file system. This is because
I wanted to wrap the program in one `escript` and to do something well-designed
with `gen_server` wasn't practical. That means that `knot:watch` is a blocking
function.

I'm outputting a .erlang file. Ideally it would be simple enough to put in `erl
-eval` but knot's API isn't what it should be. I need this big wad of code to
even get this working.

```{.erlang name="file:.erlang"}
register(dumb_watcher, spawn(fun () ->
    knot:watch(["development_environment.md", "program.md"], fun (Changed) ->
        Ends_with_erl = fun (Filename) ->
            case filename:extension(Filename) of
                ".erl" -> true;
                _ -> false
            end
        end,

        Compile = fun (Filename) ->
            io:format("Compile: ~s~n", [Filename]),
            {ok, Output} = compile:file(Filename, [{outdir, "code/ebin"}, report, verbose]),
            list_to_atom(filename:basename(Output, ".erl"))
        end,

        Reload = fun (Module) ->
            io:format("Reloading: ~s~n", [Module]),
            code:purge(Module),
            code:load_file(Module)
        end,

        Compile_and_reload = fun (Filename) ->
            Reload(Compile(Filename))
        end,

        io:format("Changed: ~p~n", [Changed]),
        Output = knot:process_files(Changed),
        Erl_files = lists:filter(Ends_with_erl, Output),
        lists:map(Compile_and_reload, Erl_files)
    end)
end)).
```

```{.Dockerfile name="file:Dockerfile"}
CMD ["erl", "-pa", "/mon/code/ebin"]
```

# Typical Docker Stuff

When a running Docker container writes files they're owned by the same uid/gid
on the host system. Since root is the default user, any files output will be
owned by root. [Until Docker supports user namespaces][], I like to run the
container as 1000/1000 which is me on my host system. It may not be on yours.
Since I'm alone, so alone, this is fine.

```{.Dockerfile name="file:Dockerfile"}
RUN mkdir /mon
RUN groupadd --gid 1000 mon
RUN useradd --home-dir /mon/.docker_home --gid 1000 --uid 1000 mon
RUN chown --recursive mon:mon /mon
WORKDIR /mon
USER mon
```

I made the home directory of the `mon` user `/mon/.docker_home`. When running
the container, I'll map the current directory to `/mon` which means that I can
create an ignored directory `.docker_home` that will have the bash/erl history
for the ultimate in convenience. It also means that I can add bash aliases if I
want. (I'm always running `ll` in containers and it's never defined.)



[knot]: https://github.com/mqsoh/knot
[Erlang from source]: http://www.erlang.org/doc/installation_guide/INSTALL.html
[the official Dockerfile]: https://github.com/c0b/docker-erlang-otp/blob/master/18/Dockerfile
[the `code` man page]: http://www.erlang.org/doc/man/code.html
[Until Docker supports user namespaces]: https://github.com/docker/docker/pull/12648
