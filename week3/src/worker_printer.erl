-module(worker_printer).

-export([start/0]).

start() ->
    Pid = spawn_link(fun() -> receive_loop() end),
    {ok, Pid}.

receive_loop() ->
    receive
        {filtered, FilteredText} ->
            io:format("Worker: ~p Text: ~s~n", [self(), binary_to_list(FilteredText)]),
            timer:sleep(rand:uniform(45) + 5),
            receive_loop();
        Message when Message /= <<"event: \"message\"">> ->
            case string:find(Message, "panic") of
                nomatch ->
                    [_, Json] = string:split(Message, ": "),
                    TweetMap = jsx:decode(Json),
                    #{<<"message">> := #{<<"tweet">> := #{<<"text">> := TweetText}}} = TweetMap,
                    filter ! {TweetText, self()},
                    receive_loop();
                _ ->
                    io:format("Worker: ~p, Kill message received (panic) ~n", [self()])
            end
    end.
