-module(reader).

-export([start/1]).

start(Endpoint) ->
    {Pid, _} = spawn_monitor(fun() -> read_sse(Endpoint) end),
    {ok, Pid}.

read_sse(Endpoint) ->
    {ok, Conn} = shotgun:open("localhost", 8080),
    Options =
        #{async => true,
          async_mode => sse,
          handle_event =>
              fun(_, _State, BinMsg) -> printer_sup:send_msg(BinMsg) end},
    {ok, _Ref} = shotgun:get(Conn, Endpoint, #{}, Options).
