-module(week6_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  start_ets(),
  week6_sup:start_link().

stop(_State) ->
  ok.

  start_ets() ->
    ets:new(tweets, [duplicate_bag, public, named_table]),
    ets:new(batched_tweets, [duplicate_bag, public, named_table]).