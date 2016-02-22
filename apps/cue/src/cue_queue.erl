%%%-------------------------------------------------------------------
%%% @author derek
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cue_queue).
-author("Derek Brown").

%% API
-export([new/0]).
-export([in/2]).
-export([out/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


new() ->
  [].

in(Queue, Value) ->
  Queue ++ [Value].

out([]) ->
  [];
out([H | T]) ->
  [{H, T}].

  

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).

new_test() ->
  ?assertMatch([], new()).

in_test() ->
  Q = new(),
  Q2 = in(Q, a),
  ?assertMatch([a], Q2),
  Q3 = in(Q2, b),
  ?assertMatch([a, b], Q3).

out_test() ->
  Q = new(),
  
  ?assertMatch([], out(Q)),
  
  Q2 = in(Q, a),
  Q3 = in(Q2, b),

  [{E, Q4}] = out(Q3),
  ?assertMatch({a, [b]}, {E, Q4}),

  [{E2, Q5}] = out(Q4),
  ?assertMatch({b, []}, {E2, Q5}).
  
-endif.

