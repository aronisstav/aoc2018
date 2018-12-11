#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/11

-define(SIZE, 300).

main(Args) ->
  {ok, [S]} = io:fread("", "~d"),
  Grid = make_grid(S),
  %print_grid(Grid),
  Sol =
    case Args of
      ["2"] -> max_all_square(Grid);
      _ -> max_square(Grid, 2)
    end,
  io:format("~p~n", [Sol]).

make_grid(S) ->
  FoldRow =
    fun({X, Y}, GridAcc) ->
        RID = X + 10,
        PW1 = RID * Y,
        PW2 = PW1 + S,
        PW3 = PW2 * RID,
        PW4 = (PW3 div 100) rem 10,
        PW = PW4 - 5,
        GridAcc#{{X,Y} => PW}
    end,
  Cells = [{X, Y} || X <- lists:seq(1, ?SIZE), Y <- lists:seq(1, ?SIZE)],
  lists:foldl(FoldRow, #{}, Cells).

%% print_grid(Grid) ->
%%   {{MinX, MinY}, {MaxX, MaxY}} = find_box(Grid),
%%   io:format("~p~n", [{{MaxX - MinX, MaxY - MinY}, {MinX, MinY}, {MaxX, MaxY}}]),
%%   FoldRow =
%%     fun(Y) ->
%%         Print =
%%           fun(X) ->
%%               C = maps:get({X, Y}, Grid),
%%               io:format("~5w", [C])
%%           end,
%%         lists:foreach(Print, [X || X <- lists:seq(MinX, MaxX)]),
%%         io:format("~n")
%%     end,
%%   lists:foreach(FoldRow, [Y || Y <- lists:seq(MinY, MaxY)]),
%%   ok.

%% find_box(Map) ->
%%   Fold =
%%     fun({X, Y}, _, {{MinX, MinY}, {MaxX, MaxY}}) ->
%%         { {min(X, MinX), min(Y, MinY)}
%%         , {max(X, MaxX), max(Y, MaxY)}
%%         }
%%     end,
%%   maps:fold(Fold, {{100000, 100000}, {0, 0}}, Map).

max_square(Grid, N) ->
  Fold =
    fun({X, Y}, MapAcc) ->
        SquareCell =
          [{XX, YY} || XX <- lists:seq(X, X + N), YY <- lists:seq(Y, Y + N)],
        Square = [maps:get({XX, YY}, Grid) || {XX, YY} <- SquareCell],
        MapAcc#{{X, Y} => lists:sum(Square)}
    end,
  SumCells =
    [{XX, YY} || XX <- lists:seq(1, ?SIZE - N), YY <- lists:seq(1, ?SIZE - N)],
  Sums = lists:foldl(Fold, #{}, SumCells),
  max_key(Sums).

max_key(Map) ->
  Fold =
    fun(K, V, {Id, Max}) ->
        case V > Max of
          true -> {K, V};
          false -> {Id, Max}
        end
    end,
  maps:fold(Fold, {0, 0}, Map).

max_all_square(Grid) ->
  Fold =
    fun(N, Acc) ->
        {Cell, Val} = max_square(Grid, N),
        %io:format("~p~n", [{N + 1, Cell, Val}]),
        io:format("."),
        Acc#{{Cell, N + 1} => Val}
    end,
  Map = lists:foldl(Fold, #{}, lists:seq(0, 19)),
  io:format("~n"),
  max_key(Map).
