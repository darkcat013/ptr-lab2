-module(profanity_filter).

-export([start/0]).

start() ->
  BadWordsMap = init_badwords_map(),
  Pid = spawn_link(fun() -> loop(BadWordsMap) end),
  register(filter, Pid),
  {ok, Pid}.

loop(BadWordsMap) ->
  receive
    {TweetText, EngagementRatio, CallerPid} ->
      FilteredText = match_words(BadWordsMap, TweetText),
      CallerPid ! {filtered, {FilteredText, EngagementRatio, TweetText /= FilteredText}}
  end,
  loop(BadWordsMap).

init_badwords_map() ->
  {ok, Binary} = file:read_file("badwords.txt"),
  init_badwords_map(#{}, string:split(Binary, "\r\n", all)).

init_badwords_map(Map, []) ->
  Map;
init_badwords_map(Map, WordList) ->
  [Word | NewList] = WordList,
  init_badwords_map(maps:put(Word, get_asterisks(Word), Map), NewList).

get_asterisks(Word) ->
  get_asterisks(byte_size(Word), "").

get_asterisks(0, Acc) ->
  Acc;
get_asterisks(WordLength, Acc) ->
  get_asterisks(WordLength - 1, "*" ++ Acc).

match_words(Dictionary, OriginalString) when is_map(Dictionary) ->
  SplitString = string:split(binary_to_list(OriginalString), " ", all),
  ResultString =
    lists:map(fun(Word) ->
                 case maps:is_key(list_to_binary(Word), Dictionary) of
                   true -> maps:get(list_to_binary(Word), Dictionary);
                   false -> Word
                 end
              end,
              SplitString),
  list_to_binary(string:join(ResultString, " ")).
