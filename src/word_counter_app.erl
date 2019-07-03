%%%-------------------------------------------------------------------
%% @doc word_counter public API
%% @end
%%%-------------------------------------------------------------------

-module(word_counter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    word_counter_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
