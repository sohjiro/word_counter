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
  count(Words).

split_in_words([], Acc) -> lists:flatten(Acc);

split_in_words([Line | Lines], Acc) ->
  Words = string:split(string:trim(Line, both), " ", all),
  split_in_words(Lines, [Words | Acc]).

count([]) -> ok;

count([Word | Words]) ->
  case clean_word(Word) of
    no_word ->
      count(Words);

    {OneWord, NewWords} ->
      word_counter_state:update_word(OneWord),
      count(NewWords ++ Words)
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

