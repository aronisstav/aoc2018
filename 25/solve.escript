#!/usr/bin/env escript
-mode(native).
%-mode(compile).

%% https://adventofcode.com/2018/day/25

main(Args) ->
  Input = read_list("~d,~d,~d,~d"),
  Answer =
    case Args of
      ["2"] -> 42;
      _ -> constellations(Input)
    end,
  io:format("~p~n", [Answer]).

read_list(Pat) ->
  read_list(Pat, 1, []).

read_list(Pat, N, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, N + 1, [{N, Res}|Acc]);
    eof -> lists:reverse(Acc)
  end.

constellations(Input) ->
  Adj = adj(Input, #{}, maps:from_list(Input)),
  length(strongly(Adj)).

adj([], Map, _) ->
  Map;
adj([{I, Loc}|Rest], Map, Points) ->
  Fold =
    fun(K, V, Acc) ->
        case manhattan(Loc, V) =< 3 of
          true -> ordsets:add_element(K, Acc);
          false -> Acc
        end
    end,
  adj(Rest, Map#{I => maps:fold(Fold, [], Points)}, Points).

manhattan([XA, YA, ZA, TA], [XB, YB, ZB, TB]) ->
  0 +
    abs(XA - XB) +
    abs(YA - YB) +
    abs(ZA - ZB) +
    abs(TA - TB).

strongly(Adj) ->
  strongly(Adj, []).

strongly(Adj, Strongly) ->
  Iter = maps:iterator(Adj),
  case maps:next(Iter) of
    none -> Strongly;
    {Key, Value, _} ->
      {NewAdj, Comp} = take_component(Value, maps:remove(Key, Adj), [Key]),
      strongly(NewAdj, [Comp|Strongly])
  end.

take_component([], Adj, Comp) ->
  {Adj, Comp};
take_component([V|R], Adj, Comp) ->
  New = maps:get(V, Adj, []),
  case New =:= [] of
    true -> take_component(R, Adj, Comp);
    false -> take_component(New ++ R, maps:remove(V, Adj), [V|Comp])
  end.
