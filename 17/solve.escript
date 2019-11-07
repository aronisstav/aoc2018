#!/usr/bin/env escript
%-mode(compile).
-mode(native).

%% https://adventofcode.com/2018/day/17

main(Args) ->
  Map = read_input(),
  Sol =
    case Args of
      ["2"] -> flood_and_stable(Map);
      _ -> flood_and_count(Map)
    end,
  io:format("~p~n", [Sol]).

read_input() ->
  Lines = read_list("~c=~d, ~c=~d..~d"),
  make_map(Lines).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

make_map(Lines) ->
  make_map(Lines, #{{0, 500} => $|}, 500, 500, 0).

make_map([], Map, MinX, MaxX, MaxY) ->
  {Map, MinX - 1, MaxX + 1, MaxY};
make_map([[C1, D1, C2, D2, D3]|Rest], Map, MinX, MaxX, MaxY) ->
  {Squares, NewMinX, NewMaxX, NewMaxY} =
    case {C1, C2} of
      {"x", "y"} ->
        { [{Y, D1} || Y <- lists:seq(D2, D3)]
        , min(MinX, D1)
        , max(MaxX, D1)
        , max(MaxY, D3)
        };
      {"y", "x"} ->
        { [{D1, X} || X <- lists:seq(D2, D3)]
        , min(MinX, D2)
        , max(MaxX, D3)
        , max(MaxY, D1)
        }
    end,
  Fold =
    fun(Where, MapAcc) ->
        MapAcc#{Where => $#}
    end,
  NewMap = lists:foldl(Fold, Map, Squares),
  make_map(Rest, NewMap, NewMinX, NewMaxX, NewMaxY).

print({Map, MinX, MaxX, MaxY}) ->
  Foreach =
    fun(Y) ->
        For =
          fun(X) ->
              C =
                case {X, Y} of
                  {696, 1309} -> $@;
                  _ -> maps:get({Y, X}, Map, $.)
                end,
              io:format("~c", [C])
          end,
        lists:foreach(For, lists:seq(MinX, MaxX)),
        io:format("~n")
    end,
  lists:foreach(Foreach, lists:seq(0, MaxY)).

flood_and_count(Map) ->
  put(c, 1),
  FinalMap = flood(queue:from_list([{0, 500}]), Map),
  print(FinalMap),
  count_all(FinalMap).

flood_and_stable(Map) ->
  put(c, 1),
  FinalMap = flood(queue:from_list([{0, 500}]), Map),
  print(FinalMap),
  count_stable(FinalMap).

flood(Q, {_, MiX, MaX, MY} = M) ->
  C = get(c),
  case C rem 1000000 =:= 0 of
    true ->
      io:format("~n~n~n~n~n"),
      print(M),
      io:format("~p~n~p~n", [queue:to_list(Q),count_all(M)]);
    false -> ok
  end,
  put(c, C + 1),
  try
    flood1(Q, M)
  catch
    C:R:S ->
      print(M),
      erlang:raise(C, R, S)
  end.

flood1(QueueIn, {Map, MiX, MaX, MY} = M) ->
  %%io:format("~n"),
  %%print(M),
  {Element, Queue} = queue:out(QueueIn),
  %%io:format("~p~n", [Element]),
  case Element of
    empty -> M;
    {value, {Y, X} = Item} ->
      case maps:get(Item, Map, $.) of
        S when S =:= $.; S =:= $#; S =:= $~ ->
          flood(Queue, M);
        $| ->
          case Y + 1 > MY of
            true ->
              flood(Queue, M);
            false ->
              Below = {Y + 1, X},
              case maps:get(Below, Map, $.) of
                $. ->
                  NewMap = Map#{Below => $|},
                  NewQueue = queue:in(Below, Queue),
                  flood(NewQueue, {NewMap, MiX, MaX, MY});
                $| ->
                  flood(Queue, M);
                S when S =:= $#; S =:= $~ ->
                  Left = {Y, X - 1},
                  Right = {Y, X + 1},
                  {NQ1, NM1} = flood_hor(left, Left, Queue, Map),
                  {NQ2, NM2} = flood_hor(right, Right, NQ1, NM1),
                  flood(NQ2, {NM2, MiX, MaX, MY})
              end
          end
      end
  end.

flood_hor(Dir, {Y, X} = C, Q, M) ->
  CheckDir =
    case Dir of
      left -> fun({YY, XX}) -> {YY, XX + 1} end;
      right -> fun({YY, XX}) -> {YY, XX - 1} end
    end,
  case maps:get(C, M, $.) of
    $. ->
      NQ = queue:in(C, Q),
      NM = M#{C => $|},
      {NQ, NM};
    $# ->
      check_wall(CheckDir, CheckDir(C), Q, M);
    $| ->
      find_edge(CheckDir, C, Q, M);
    _ -> {Q, M}
  end.

find_edge(CheckDir, C, Q, M) ->
  CC = CheckDir(C),
  case maps:get(CC, M, $.) of
    $# ->
      {queue:in(C, Q), M};
    $| ->
      find_edge(CheckDir, CC, Q, M);
    _ -> {Q, M}
  end.

check_wall(CheckDir, C, Q, M) ->
  case has_wall(CheckDir, C, M) of
    false -> {Q, M};
    true -> make_stable(CheckDir, C, Q, M)
  end.

has_wall(Fun, C, M) ->
  NC = Fun(C),
  case maps:get(NC, M, $.) of
    $# -> true;
    $| -> has_wall(Fun, NC, M);
    _ -> false
  end.

make_stable(Fun, {Y, X} = C, Q, M) ->
  case maps:get(C, M, $.) of
    $# -> {Q, M};
    $~ -> {Q, M};
    $| ->
      NM = M#{C => $~},
      NQ = queue:in({Y - 1, X}, Q),
      make_stable(Fun, Fun(C), NQ, NM)
  end.

count_all({Map, _, _, _}) ->
  Fold =
    fun(_, V, Acc) ->
        case V of
          $| -> Acc + 1;
          $~ -> Acc + 1;
          _ -> Acc
        end
    end,
  maps:fold(Fold, 0, Map).

count_stable({Map, _, _, _}) ->
  Fold =
    fun(_, V, Acc) ->
        case V of
          $~ -> Acc + 1;
          _ -> Acc
        end
    end,
  maps:fold(Fold, 0, Map).
