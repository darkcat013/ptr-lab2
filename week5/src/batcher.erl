-module(batcher).

-export([start/1]).

start(BatchSize) ->
  Pid = spawn_link(fun() -> loop(BatchSize, []) end),
  register(batcher, Pid),
  {ok, Pid}.

loop(BatchSize, Batch) ->
  case length(Batch) == BatchSize of
    true ->
      print_tweets(lists:reverse(Batch)),
      loop(BatchSize, []);
    false ->
      receive
        {batch, Params} ->
          NewBatch = [Params | Batch],
          loop(BatchSize, NewBatch)
      end
  end.

print_tweets([]) ->
  ok;
print_tweets(Batch) ->
  [Params | NewBatch] = Batch,
  {FilteredText, EngagementRatio, IsRedacted, SentimentScore} = Params,
  io:format("BATCH Worker: ~p; Sentiment Score: ~p; Engagement Ratio: ~p; Filtered: ~p Text: "
                    "~s~n",
                    [self(), SentimentScore, EngagementRatio, IsRedacted, binary_to_list(FilteredText)]),
  print_tweets(NewBatch).
