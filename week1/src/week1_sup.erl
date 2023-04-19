-module(week1_sup).

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

    PrinterSup =
        #{id => printer_sup,
          start => {printer_sup, start_link, []},
          restart => permanent,
          shutdown => 2000,
          type => supervisor,
          modules => [printer_sup]},

    ChildSpecs = [ReaderSup, PrinterSup],
    {ok, {SupFlags, ChildSpecs}}.
