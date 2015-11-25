FROM erlang:18
RUN git clone https://github.com/mqsoh/knot.git /usr/local/lib/erlang/lib/knot
RUN cd /usr/local/lib/erlang/lib/knot && erl -noshell -s make all -s init stop
CMD ["erl", "-pa", "/mon/code/ebin"]
RUN mkdir /mon
RUN groupadd --gid 1000 mon
RUN useradd --home-dir /mon/.docker_home --gid 1000 --uid 1000 mon
RUN chown --recursive mon:mon /mon
WORKDIR /mon
USER mon