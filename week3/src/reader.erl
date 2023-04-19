-module(reader).

-export([start/1]).

start(Endpoint) ->
    Pid = spawn_link(fun() -> read_sse(Endpoint) end),
    {ok, Pid}.

read_sse(Endpoint) ->
    {ok, Conn} = shotgun:open("localhost", 8080),
    Options =
        #{async => true,
          async_mode => sse,
          handle_event => fun(_, _State, BinMsg) -> balancer ! BinMsg end},
    {ok, _Ref} = shotgun:get(Conn, Endpoint, #{}, Options),
    timer:sleep(100000),
    shotgun:close(Conn).
