%%%-------------------------------------------------------------------
%% @doc week1 public API
%% @end
%%%-------------------------------------------------------------------

-module(week1_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    week1_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
