-module(worker_pool_sup).

-behaviour(supervisor).

-export([start_link/1, send_msg/2]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link(WorkersNr) ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    start_workers(WorkersNr),
    {ok, Pid}.

init([]) ->
    MaxRestarts = 1000,
    MaxTime = 10,
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => MaxRestarts,
          period => MaxTime},

    ChildSpecs =
        [#{id => worker_printer,
           start => {worker_printer, start, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [worker_printer]}],
    {ok, {SupFlags, ChildSpecs}}.

send_msg(BinMsg, WorkerNr) ->
    {_, ChildPid, _, _} = lists:nth(WorkerNr, supervisor:which_children(?MODULE)),
    ChildPid ! BinMsg.

new_child() ->
    supervisor:start_child(?MODULE, []).

start_workers(0) -> ok;
start_workers(WorkersNr) ->
    new_child(),
    start_workers(WorkersNr-1).
