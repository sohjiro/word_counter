%%%-------------------------------------------------------------------
%% @doc word_counter_process linear module
%% @end
%%%-------------------------------------------------------------------

-module(word_counter_process).

-export([count_words/1]).

count_words(FileName) ->
  {ok, Content} = file:read_file(FileName),
  Lines = string:split(Content, "\n", all),
  Words = split_in_words(Lines, []),
  count(Words, maps:new()).

split_in_words([], Acc) -> lists:flatten(Acc);

split_in_words([Line | Lines], Acc) ->
  Words = string:split(string:trim(Line, both), " ", all),
  split_in_words(Lines, [Words | Acc]).

count([], Counter) -> Counter;

count([Word | Words], Counter) ->
  Counter_Fun = fun(V) -> V + 1 end,

  case clean_word(Word) of
    no_word ->
      count(Words, Counter);

    {OneWord, NewWords} ->
      NewAcc = maps:update_with(OneWord, Counter_Fun, 1, Counter),
      count(NewWords ++ Words, NewAcc)
  end.

clean_word(Word) ->
  NewWord = re:replace(Word, "\\W", " ", [global, {return, binary}]),
  Words = string:split(string:trim(NewWord, both), " ", all),
  case Words of
    [OneWord] ->
      {OneWord, []};

    [OneWord | Rest] ->
      {OneWord, Rest};

    _ ->
      no_word
  end.

