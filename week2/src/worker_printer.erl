-module(worker_printer).

-export([start/0]).

start() ->
    Pid = spawn_link(fun() -> receive_loop() end),
    {ok, Pid}.

receive_loop() ->
    receive
        Message when <<"event: \"message\"">> /= Message ->
            case string:find(Message, "panic") of
                nomatch ->
                    [_, Json] = string:split(Message, ": "),
                    TweetMap = jsx:decode(Json),
                    #{<<"message">> := #{<<"tweet">> := #{<<"text">> := TweetText}}} = TweetMap,
                    io:format("Worker: ~p, Text: ~s~n", [self(), binary_to_list(TweetText)]),
                    timer:sleep(rand:uniform(45) + 5),
                    receive_loop();
                _ ->
                    io:format("Worker: ~p, Kill message received (panic) ~n", [self()])
            end
    end.
