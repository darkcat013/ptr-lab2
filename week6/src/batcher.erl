-module(batcher).

-export([start/1]).

start(BatchSize) ->
  Pid = spawn_link(fun() -> loop(BatchSize, []) end),
  register(batcher, Pid),
  {ok, Pid}.

loop(BatchSize, Batch) ->
  case length(Batch) == BatchSize of
    true ->
      ets:insert_new(batched_tweets, Batch),
      loop(BatchSize, []);
    false ->
      receive
        {batch, Params} ->
          NewBatch = [Params | Batch],
          loop(BatchSize, NewBatch)
      end
  end.
