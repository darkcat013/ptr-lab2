-module(reader_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    MaxRestarts = 1000,
    MaxTime = 10,
    SupFlags =
        #{strategy => one_for_one,
          intensity => MaxRestarts,
          period => MaxTime},

    Endpoint1 = "/tweets/1",
    Endpoint2 = "/tweets/2",

    Reader1 =
        #{id => reader1,
          start => {reader, start, [Endpoint1]},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [reader]},

    Reader2 =
        #{id => reader2,
          start => {reader, start, [Endpoint2]},
          restart => permanent,
          shutdown => 2000,
          type => worker,
          modules => [reader]},

    ChildSpecs = [Reader1, Reader2],
    {ok, {SupFlags, ChildSpecs}}.
