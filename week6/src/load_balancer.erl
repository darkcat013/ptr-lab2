-module(load_balancer).

-export([start/1]).

start(WorkersNr) ->
  Pid = spawn_link(fun() -> balancer_loop(WorkersNr, 1) end),
  register(balancer, Pid),
  {ok, Pid}.

%% round robin
balancer_loop(WorkersNr, CurrentNr) ->
  receive
    BinMsg ->
      worker_pool_sup:send_msg(BinMsg, CurrentNr)
  end,
  balancer_loop(WorkersNr, CurrentNr rem 3 + 1).
