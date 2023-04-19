-module(worker_printer).

-export([start/0]).

start() ->
    Pid = spawn_link(fun() -> receive_loop() end),
    {ok, Pid}.

receive_loop() ->
    receive
        {sentiment_calculated, Params} -> 
            {FilteredText, EngagementRatio, SentimentScore} = Params,
            io:format("Worker: ~p; Sentiment Score: ~p; Engagement Ratio: ~p; Text: ~s~n", [self(), SentimentScore, EngagementRatio, binary_to_list(FilteredText)]),
            timer:sleep(rand:uniform(45) + 5),
            receive_loop();
        {filtered, Params} ->
            sentiment ! {Params, self()},
            receive_loop();
        Message when Message /= <<"event: \"message\"">> ->
            case string:find(Message, "panic") of
                nomatch ->
                    [_, Json] = string:split(Message, ": "),
                    TweetMap = jsx:decode(Json),
                    #{<<"message">> := #{<<"tweet">> := #{<<"text">> := TweetText}}} = TweetMap,
                    #{<<"message">> := #{<<"tweet">> := #{<<"favorite_count">> := Favorites}}} = TweetMap,
                    #{<<"message">> := #{<<"tweet">> := #{<<"retweet_count">> := Retweets}}} = TweetMap,
                    #{<<"message">> := #{<<"tweet">> := #{<<"user">> := #{<<"followers_count">> := Followers}}}} = TweetMap,
                    EngagementRatio = (Favorites + Retweets)/Followers,
                    filter ! {TweetText, EngagementRatio, self()},
                    receive_loop();
                _ ->
                    io:format("Worker: ~p, Kill message received (panic) ~n", [self()])
            end
    end.
