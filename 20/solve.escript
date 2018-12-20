#!/usr/bin/env escript
%-mode(native).
-mode(compile).

%% https://adventofcode.com/2018/day/20

main(Args) ->
  Input = io:get_line(""),
  Map = map_expr(Input),
  Answer =
    case Args of
      ["2"] -> more_than_1000(Map);
      _ -> furthest(Map)
    end,
  io:format("~p~n", [Answer]).

map_expr([$^|Rest]) ->
  map_expr(Rest, {0, 0}, [], fix_room({0, 0}, #{})).

map_expr([C|Rest], P, Stack, Map) ->
  case C of
    $$ ->
      Fold =
        fun(K, V, Acc) ->
            case V =:= $? of
              true -> Acc#{K => $#};
              false -> Acc#{K => V}
            end
        end,
      maps:fold(Fold, #{}, Map);
    L when L =:= $N; L =:= $E; L =:= $S; L =:= $W ->
      {NP, NMap} = move(P, Map, L),
      map_expr(Rest, NP, Stack, NMap);
    $| ->
      [H|_] = Stack,
      map_expr(Rest, H, Stack, Map);
    $( ->
      map_expr(Rest, P, [P|Stack], Map);
    $) ->
      [H|R] = Stack,
      map_expr(Rest, H, R, Map)
  end.

move({Y, X} = P, Map, L) ->
  NMap = fix_room(P, Map),
  {Door, NP, DS} =
    case L of
      $N -> {{Y - 1, X}, {Y - 2, X}, $-};
      $S -> {{Y + 1, X}, {Y + 2, X}, $-};
      $W -> {{Y, X - 1}, {Y, X - 2}, $|};
      $E -> {{Y, X + 1}, {Y, X + 2}, $|}
    end,
  DMap = update_map(Door, DS, NMap),
  FMap = fix_room(NP, DMap),
  {NP, FMap}.

fix_room({Y, X}, Map) ->
  Ways = [{Y - 1, X}, {Y, X - 1}, {Y, X + 1}, {Y + 1, X}],
  Walls = [{Y - 1, X - 1}, {Y - 1, X + 1}, {Y + 1, X - 1}, {Y + 1, X + 1}],
  FoldWays =
    fun(C, Acc) ->
        update_map(C, $?, Acc)
    end,
  WayMap = lists:foldl(FoldWays, Map, Ways),
  FoldWalls =
    fun(C, Acc) ->
        update_map(C, $#, Acc)
    end,
  lists:foldl(FoldWalls, WayMap#{{Y, X} => $.}, Walls).

update_map(P, S, Map) ->
  case maps:get(P, Map, $?) of
    U when U =:= $? ->
      Map#{P => S};
    _ ->
      Map
  end.

%% print(Map) ->
%%   Fold =
%%     fun({Y, X}, _, {{MiY, MiX}, {MaY, MaX}}) ->
%%         {{min(Y, MiY), min(X, MiX)}, {max(Y, MaY), max(X, MaX)}}
%%     end,
%%   {{MiY, MiX}, {MaY, MaX}} = maps:fold(Fold, {{0,0}, {0,0}}, Map),
%%   Foreach =
%%     fun(Y) ->
%%         For =
%%           fun(X) ->
%%               C =
%%                 case {X, Y} of
%%                   _ -> maps:get({Y, X}, Map, $.)
%%                 end,
%%               io:format("~c", [C])
%%           end,
%%         lists:foreach(For, lists:seq(MiX, MaX)),
%%         io:format("~n")
%%     end,
%%   lists:foreach(Foreach, lists:seq(MiY, MaY)).

furthest(Map) ->
  DMap = furthest(queue:from_list([{{0, 0}, 0}]), Map, #{}),
  lists:max(maps:values(DMap)).

furthest(Queue, Map, DMap) ->
  {V, Q} = queue:out(Queue),
  case V of
    empty -> DMap;
    {value, {C, D}} ->
      {NQueue, NDMap} =
        case maps:get(C, DMap, no) =:= no of
          true ->
            NQ = neighs(C, D, Map, Q),
            NDM = DMap#{C => D},
            {NQ, NDM};
          false ->
            {Q, DMap}
        end,
      furthest(NQueue, Map, NDMap)
  end.

neighs({Y, X}, D, Map, Q) ->
  Ways =
    [ {{Y - 1, X}, {Y - 2, X}}
    , {{Y, X - 1}, {Y, X - 2}}
    , {{Y, X + 1}, {Y, X + 2}}
    , {{Y + 1, X}, {Y + 2, X}}
    ],
  Fold =
    fun({MD, MN}, QAcc) ->
        case maps:get(MD, Map, $#) =/= $# of
          true ->
            queue:in({MN, D + 1}, QAcc);
          false ->
            QAcc
        end
    end,
  lists:foldl(Fold, Q, Ways).

more_than_1000(Map) ->
  DMap = furthest(queue:from_list([{{0, 0}, 0}]), Map, #{}),
  length([D || D <- maps:values(DMap), D >= 1000]).
