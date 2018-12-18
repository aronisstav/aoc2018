#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/14

-define(INPUT, 635041).

main(Args) ->
  Sol =
    case Args of
      ["2"] -> seek([6,3,5,0,4,1]);
      _ ->
        {_, P} = score(?INPUT),
        P
    end,
  io:format("~p~n", [Sol]).

seek(Target) ->
  seek(Target, 1000).

seek(Target, Range) ->
  io:format("~p~n", [Range]),
  {M, _} = score(Range),
  io:format("Score done~n"),
  case find(Target, M) of
    {true, I} -> I;
    false -> seek(Target, Range * 10)
  end.

find(Target, M) ->
  Rs = [D || {_, D} <- lists:sort(maps:to_list(M))],
  io:format("Seq Done"),
  find(0, Rs, Target).

find(_, [], _) ->
  false;
find(I, [_|RRs] = Rs, Ds) ->
  case lists:prefix(Ds, Rs) of
    true ->
      {true, I};
    false ->
      find(I + 1, RRs, Ds)
  end.

score(N) ->
  score(0, 1, #{0 => 3, 1 => 7}, 2, N).

score(_, _, Map, M, N) when M > N + 11 ->
  Fold =
    fun(X, A) ->
        A * 10 + maps:get(X, Map)
    end,
  {Map, lists:foldl(Fold, 0, lists:seq(N, N + 9))};
score(E1, E2, Map, M, N) ->
  %io:format("~p~n", [[D || {_, D} <- lists:sort(maps:to_list(Map))]]),
  S1 = maps:get(E1, Map),
  S2 = maps:get(E2, Map),
  New = S1 + S2,
  %io:format("~p~n", [{E1, E2, S1, S2, New}]),
  {NewMap, NewM} =
    case New > 9 of
      true ->
        {Map#{M => New div 10, M + 1 => New rem 10}, M + 2};
      false ->
        {Map#{M => New}, M + 1}
    end,
  {NewE1, NewE2} =
    {(E1 + S1 + 1) rem NewM, (E2 + S2 + 1) rem NewM},
  score(NewE1, NewE2, NewMap, NewM, N).
