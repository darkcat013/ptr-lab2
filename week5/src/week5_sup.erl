-module(week5_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  MaxRestarts = 3,
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

  ProfanityFilter =
    #{id => profanity_filter,
      start => {profanity_filter, start, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [profanity_filter]},

  SentimentScore =
    #{id => sentiment_score,
      start => {sentiment_score, start, []},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [sentiment_score]},

  BatchSize = 5,
  Batcher =
    #{id => batcher,
      start => {batcher, start, [BatchSize]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [batcher]},

  ChildSpecs = [ReaderSup, WorkerPoolSup, LoadBalancer, ProfanityFilter, SentimentScore, Batcher],
  {ok, {SupFlags, ChildSpecs}}.
