-module(printer).

-export([start/0]).

start() ->
    {Pid, _} = spawn_monitor(fun() -> receive_loop() end),
    {ok, Pid}.

receive_loop() ->
    receive
        Message when <<"event: \"message\"">> /= Message ->
            [_ | [Json]] = string:split(Message, ": "),
            TweetMap = jsx:decode(Json),
            #{<<"message">> := #{<<"tweet">> := #{<<"text">> := TweetText}}} = TweetMap,
            io:format("~s~n", [binary_to_list(TweetText)]),
            timer:sleep(rand:uniform(45)+5)
    end,
    receive_loop().
