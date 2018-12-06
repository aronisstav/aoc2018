#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/6

main(Args) ->
  Input = read_list("~d, ~d"),
  Sol =
    case Args of
      ["2"] -> safe_area(Input);
      _ -> area(Input)
    end,
  io:format("~s~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

area(Input) ->
  {MaxX, MaxY} = find_maxs(Input),
  Labeled = label(Input),
  {Map, Counts} = count_area(MaxX, MaxY, Labeled),
  Bordering = bordering(Map, MaxX, MaxY),
  FiniteMap = maps:without([tie|Bordering], Counts),
  Ans = lists:max(maps:values(FiniteMap)),
  io_lib:format("~p", [Ans]).

find_maxs(Pairs) ->
  Fold =
    fun([X, Y], {MaxX, MaxY}) ->
        {max(X, MaxX), max(Y, MaxY)}
    end,
  lists:foldl(Fold, {0, 0}, Pairs).

label(Input) ->
  Fold =
    fun([X, Y], {N, Acc}) ->
        {N + 1, [{{X, Y}, N}|Acc]}
    end,
  {_, R} = lists:foldl(Fold, {1, []}, Input),
  R.

count_area(MaxX, MaxY, Labeled) ->
  Coords = [{X, Y} || X <- lists:seq(0, MaxX), Y <- lists:seq(0, MaxY)],
  assign_and_count(Coords, Labeled).

assign_and_count(Coords, Labeled) ->
  assign_and_count(Coords, Labeled, #{}, #{}).

assign_and_count([], _, Map, Counts) ->
  {Map, Counts};
assign_and_count([{X, Y}|Rest], Labeled, Map, Counts) ->
  L = find_nearest({X, Y}, Labeled),
  NewMap = Map#{{X, Y} => L},
  Inc = fun(V) -> V + 1 end,
  NewCounts = maps:update_with(L, Inc, 1, Counts),
  assign_and_count(Rest, Labeled, NewMap, NewCounts).

find_nearest(Pair, [{LPair, L}|R]) ->
  D = manhattan(Pair, LPair),
  find_nearest(Pair, R, D, L).

manhattan({X, Y}, {Z, W}) ->
  abs(X - Z) + abs(Y - W).

find_nearest(_, [], _Min, MinL) ->
  MinL;
find_nearest(Pair, [{LPair, L}|Rest], Min, MinL) ->
  D = manhattan(Pair, LPair),
  {NewMin, NewMinL} =
    if D < Min -> {D, L};
       D == Min -> {D, tie};
       true -> {Min, MinL}
    end,
  find_nearest(Pair, Rest, NewMin, NewMinL).

bordering(Map, MaxX, MaxY) ->
  Top = [{0, Y} || Y <- lists:seq(0, MaxY)],
  Bottom = [{MaxX, Y} || Y <- lists:seq(0, MaxY)],
  Left = [{X, 0} || X <- lists:seq(0, MaxX)],
  Right = [{X, MaxY} || X <- lists:seq(0, MaxX)],
  Fold =
    fun(Pair, Set) ->
        L = maps:get(Pair, Map),
        sets:add_element(L, Set)
    end,
  Borders = Top ++ Bottom ++ Left ++ Right,
  sets:to_list(lists:foldl(Fold, sets:new(), Borders)).

-define(THRESHOLD, 10000).
%% -define(PROBE, {4,3}).

safe_area(Input) ->
  {MaxX, MaxY} = find_maxs(Input),
  Points = [{X, Y} || [X, Y] <- Input],
  Map = make_map({0, 0}, {MaxX + 1, MaxY + 1}, Points, ?THRESHOLD),
  %% print_map(Map, {0, 0}, {MaxX + 1, MaxY + 1}, Points, ?THRESHOLD),
  Fold =
    fun(_, V, C) when V < ?THRESHOLD -> C + 1;
       (_, _, C) -> C
    end,
  Area = maps:fold(Fold, 0, Map),
  io_lib:format("~w", [Area]).

make_map({MinX, MinY}, {MaxX, MaxY}, Points, _Threshold) ->
  FoldRows =
    fun(Y, MapAcc) ->
        add_pairs([{X, Y} || X <- lists:seq(MinX, MaxX)], Points, MapAcc)
    end,
  _NewMap = lists:foldl(FoldRows, #{}, lists:seq(MinY, MaxY)).

add_pairs(Pairs, Points, Map) ->
  Fold =
    fun(Pair, MapAcc) ->
        TD = total_manhattan(Pair, Points),
        MapAcc#{Pair => TD}
    end,
  lists:foldl(Fold, Map, Pairs).

total_manhattan(Pair, Points) ->
  Fold = fun(Point, C) ->
             M = manhattan(Pair, Point),
             C + M
         end,
  lists:foldl(Fold, 0, Points).

%% print_map(Map, {MinX, MinY}, {MaxX, MaxY}, Points, Threshold) ->
%%   PrintRows =
%%     fun(Y) ->
%%         Print =
%%           fun(X) ->
%%               D = maps:get({X, Y}, Map),
%%               P = lists:member({X, Y}, Points),
%%               C =
%%                 if {X, Y} =:= ?PROBE -> $@;
%%                    P -> $!;
%%                    D =< Threshold -> $#;
%%                    true -> $.
%%                 end,
%%               io:format("~c", [C])
%%           end,
%%         lists:foreach(Print, lists:seq(MinX, MaxX)),
%%         io:format("~n")
%%     end,
%%   lists:foreach(PrintRows, lists:seq(MinY, MaxY)).
