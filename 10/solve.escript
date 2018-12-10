#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/10

main(Args) ->
  Input = read_list("position=<~d,~d> velocity=<~d,~d>"),
  Fold =
    fun([X, Y, VX, VY], {N, Map}) ->
        {N + 1, Map#{N => {{X, Y}, {VX, VY}}}}
    end,
  {_, Data} = lists:foldl(Fold, {1, #{}}, Input),
  Sol =
    case Args of
      ["2"] -> "TODO";
      _ -> wait_message(Data)
    end,
  io:format("~p~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

-define(START, 10513).
-define(STEP, 1).
-define(END, 10517).
-define(REFRESH, 800).

wait_message(Data) ->
  wait_message(Data, 0).

wait_message(_Data, Time) when Time > ?END -> ok;
wait_message(Data, Time) ->
  case Time >= ?START of
    true ->
      io:format("Time ~p:~n", [Time]),
      print_map(Data),
      receive after ?REFRESH -> ok end;
    false ->
      ok
  end,
  NewData = advance(Data, ?STEP),
  wait_message(NewData, Time + ?STEP).

print_map(Data) ->
  ToPoints =
    fun(_, {Point, _}, Grid) ->
        Grid#{Point => true}
    end,
  Grid = maps:fold(ToPoints, #{}, Data),
  print_grid(Grid).

print_grid(Grid) ->
  {{MinX, MinY}, {MaxX, MaxY}} = find_box(Grid),
  io:format("~p~n", [{{MaxX - MinX, MaxY - MinY}, {MinX, MinY}, {MaxX, MaxY}}]),
  FoldRow =
    fun(Y) ->
        Print =
          fun(X) ->
              C =
                case maps:get({X, Y}, Grid, false) of
                  false -> $ ;
                  true -> $#
                end,
              io:format("~c", [C])
          end,
        lists:foreach(Print, [X || X <- lists:seq(MinX, MaxX)]),
        io:format("~n")
    end,
  lists:foreach(FoldRow, [Y || Y <- lists:seq(MinY, MaxY)]),
  ok.

find_box(Map) ->
  Fold =
    fun({X, Y}, true, {{MinX, MinY}, {MaxX, MaxY}}) ->
        { {min(X, MinX), min(Y, MinY)}
        , {max(X, MaxX), max(Y, MaxY)}
        }
    end,
  maps:fold(Fold, {{100000, 100000}, {0, 0}}, Map).

advance(Map, Step) ->
  Fold =
    fun(K, {{X, Y}, {VX, VY}}, MapAcc) ->
        MapAcc#{K => {{X + Step * VX, Y + Step * VY}, {VX, VY}}}
    end,
  maps:fold(Fold, #{}, Map).
