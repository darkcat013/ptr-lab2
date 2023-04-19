-module(printer_sup).

-behaviour(supervisor).

-export([start_link/0, send_msg/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, Pid} = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
    new_child(),
    {ok, Pid}.

init([]) ->
    MaxRestarts = 1000,
    MaxTime = 10,
    SupFlags =
        #{strategy => simple_one_for_one,
          intensity => MaxRestarts,
          period => MaxTime},

    ChildSpecs =
        [#{id => printer_worker,
           start => {printer, start, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [printer]}],
    {ok, {SupFlags, ChildSpecs}}.

send_msg(BinMsg) ->
    [{_, ChildPid, _, _} | _] = supervisor:which_children(?MODULE),
    ChildPid ! BinMsg.

new_child() ->
    supervisor:start_child(?MODULE, []).