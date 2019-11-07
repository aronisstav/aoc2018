#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/15

main(Args) ->
  Lines = read_lines(),
  Map = make_map(Lines),
  Sol =
    case Args of
      ["2"] -> 42;
      _ -> final_score(Map)
    end,
  io:format("~p~n", [Sol]).

read_lines() ->
  read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Line -> read_lines([Line|Acc])
  end.

make_map(Lines) ->
  make_map(Lines, 0, 0, #{}, [], 1).

make_map([], Y, X, Map, Units, _UC) ->
  {Units, Map, Y - 1, X - 2};
make_map([Line|Rest], Y, MX, MapIn, UnitsIn, UCIn) ->
  Fold =
    fun(S, {X, Map, Units, UC}) ->
        NewMap =
          case S of
            H when H =:= $.; H =:= $E; H =:= $G -> Map#{{Y, X} => $.};
            $# -> Map#{{Y, X} => S};
            $\n -> Map
          end,
        {NewUnits, NewUC} =
          case S =:= $E orelse S =:= $G of
            %% 760089964
            true -> {orddict:store({Y, X}, {UC, S, 200}, Units), UC + 1};
            false -> {Units, UC}
          end,
        {X + 1, NewMap, NewUnits, NewUC}
    end,
  {X, NewMap, NewUnits, NewUC} =
    lists:foldl(Fold, {0, MapIn, UnitsIn, UCIn}, Line),
  make_map(Rest, Y + 1, max(MX, X), NewMap, NewUnits, NewUC).

print({Units, Map, MY, MX}) ->
  Foreach =
    fun(Y) ->
        For =
          fun(X) ->
              case orddict:find({Y, X}, Units) of
                {ok, {_, V, _}} ->
                  io:format("~c", [V]);
                error ->
                  case maps:get({Y,X}, Map, $#) of
                    $# -> io:format("#");
                    $. -> io:format(".");
                    N  -> io:format("~p", [N])
                  end
              end
          end,
          lists:foreach(For, lists:seq(0, MX)),
          io:format("~n")
    end,
  lists:foreach(Foreach, lists:seq(0, MY)).

final_score(Map) ->
  final_score(0, Map).

final_score(Turn, Map) ->
  print(Map),
  {Units, _, _, _} = Map,
  io:format("~p~n", [{Turn, Units}]),
  io:format("~n"),
%%  timer:sleep(500),
  case step(Map) of
    {won, Total} ->
      io:format("~p~n", [{Turn, Total}]),
      Turn * Total;
    {fight, NewMap} ->
      final_score(Turn + 1, NewMap)
  end.

won(Units) ->
  Fold =
    fun({_, {_, C, HP}}, Acc) ->
        V = maps:get(C, Acc, 0),
        Acc#{C => V + HP}
    end,
  Totals = lists:foldl(Fold, #{}, Units),
  case maps:to_list(Totals) of
    [{_, T}] -> {true, T};
    _ -> false
  end.

step({Units, Map, MY, MX} = M) ->
  case step(Units, M, []) of
    {fight, NewUnits} -> {fight, {NewUnits, Map, MY, MX}};
    {won, _} = Won -> Won
  end.

step([], _M, NewUnits) ->
  {fight, NewUnits};
step([{{Y, X} = Where, {_, C, _} = Who}|Rest] = Wait, Map, NewUnits) ->
  case won(Wait ++ NewUnits) of
    {true, HP} -> {won, HP};
    false ->
      case attack_neigbour(Y, X, C, Rest, NewUnits) of
        {yes, NewRest, NewNewUnits} ->
          step(NewRest, Map, orddict:store(Where, Who, NewNewUnits));
        no ->
          {YY, XX} = NewWhere = move(Y, X, C, Map, Rest, NewUnits),
          case attack_neigbour(YY, XX, C, Rest, NewUnits) of
        {yes, NewRest, NewNewUnits} ->
              step(NewRest, Map, orddict:store(NewWhere, Who, NewNewUnits));
            no ->
              step(Rest, Map, orddict:store(NewWhere, Who, NewUnits))
          end
      end
  end.

attack_neigbour(Y, X, C, Units, MoreUnits) ->
  Neighbours = [{Y - 1, X}, {Y, X - 1}, {Y, X + 1}, {Y + 1, X}],
  Fold =
    fun({Where, {N, CC, HP}}, Acc) ->
        case CC =/= C andalso lists:member(Where, Neighbours) of
          true -> orddict:store({HP, Y, X}, N, Acc);
          false -> Acc
        end
    end,
  case lists:foldl(Fold, [], Units ++ MoreUnits) of
    [{{_, _, _}, N}|_] ->
      FoldHit =
        fun({Where, {NN, CC, HP}}, Acc) when NN =:= N ->
            NHP = HP - 3,
            case NHP > 0 of
              true -> [{Where, {NN, CC, NHP}}|Acc];
              false -> Acc
            end;
           (Unit, Acc) -> [Unit|Acc]
        end,
      NewUnits = lists:foldr(FoldHit, [], Units),
      NewMoreUnits = lists:foldr(FoldHit, [], MoreUnits),
      {yes, NewUnits, NewMoreUnits};
    [] ->
      no
  end.

move(Y, X, C, {_, Map, _MY, _MX}, Units, MoreUnits) ->
  %% io:format("~c @ {~p,~p}~n", [C, X, Y]),
  AdjMap = make_adj_map_1(C, Map, Units ++ MoreUnits),
  %% print({orddict:merge(fun(_, V, _) -> V end, Units, MoreUnits), AdjMap, _MY, _MX}),
  %% io:format("~n"),
  Neighbours = [{Y - 1, X}, {Y, X - 1}, {Y, X + 1}, {Y + 1, X}],
  Fold =
    fun({YY, XX} = Where, Acc) ->
        case maps:get(Where, AdjMap, unreach) of
          unreach -> Acc;
          N -> ordsets:add_element({N, YY, XX}, Acc)
        end
    end,
  case lists:foldl(Fold, [], Neighbours) of
    [{_, YY, XX}|_] = _Q ->
      %% io:format("~p~n", [_Q]),
      {YY, XX};
    [] -> {Y, X}
  end.

make_adj_map_1(C, Map, Units) ->
  %% io:format("~p~n", [{C, Units}]),
  Split = fun({_, {_, CC, _}}) -> C =:= CC end,
  {Allies, Enemies} = lists:partition(Split, Units),
  WallAllies = fun({Where, _}, Acc) -> Acc#{Where => $#} end,
  WallAllyMap = lists:foldl(WallAllies, Map, Allies),
  ZeroEnemies = fun({Where, _}, Acc) -> Acc#{Where => 0} end,
  InitAdj = lists:foldl(ZeroEnemies, #{}, Enemies),
  Range = queue:from_list([{W, 0} || {W, _} <- Enemies]),
  %% io:format("~p~n", [{InitAdj, Range}]),
  make_adj_map(Range, WallAllyMap, InitAdj).

make_adj_map(Queue, Map, AdjMapIn) ->
  {Res, QueueIn} = queue:out(Queue),
  case Res of
    empty -> AdjMapIn;
    {value, {{Y, X}, N}} ->
      Neighbours = [{Y - 1, X}, {Y, X - 1}, {Y, X + 1}, {Y + 1, X}],
      Fold =
        fun(W, {Q, AM} = Acc) ->
            case maps:get(W, AM, no) of
              no ->
                case maps:get(W, Map, $#) of
                  $# -> Acc;
                  $. ->
                    NQ = queue:in({W, N + 1}, Q),
                    NAM = AM#{W => N + 1},
                    {NQ, NAM}
                end;
              _ -> Acc
            end
        end,
      {NQ, NAM} = lists:foldl(Fold, {QueueIn, AdjMapIn}, Neighbours),
      make_adj_map(NQ, Map, NAM)
  end.
