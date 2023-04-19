-module(week2_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    MaxRestarts = 10,
    MaxTime = 1,
    SupFlags =
        #{strategy => one_for_all,
          intensity => MaxRestarts,
          period => MaxTime},

    ReaderSup =
        #{id => reader_sup,
          start => {reader_sup, start_link, []},
          restart => permanent,
          shutdown => 2000,
          type => supervisor,
          modules => [reader_sup]},

    WorkersNr = 3,

    WorkerPoolSup =
        #{id => worker_pool_sup,
          start => {worker_pool_sup, start_link, [WorkersNr]},
          restart => permanent,
          shutdown => 2000,
          type => supervisor,
          modules => [worker_pool_sup]},

    LoadBalancer =
        #{id => load_balancer,
          start => {load_balancer, start, [WorkersNr]},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [load_balancer]},

    ChildSpecs = [ReaderSup, WorkerPoolSup, LoadBalancer],
    {ok, {SupFlags, ChildSpecs}}.
