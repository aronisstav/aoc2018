#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/18

main(Args) ->
  Lines = read_lines(),
  Map = make_map(Lines),
  Sol =
    case Args of
      ["2"] -> score(evolve(1000000000, Map));
      _ -> score(evolve(10, Map))
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
  make_map(Lines, 0, 0, #{}).

make_map([], Y, X, Map) ->
  {Map, Y - 1, X - 2};
make_map([Line|Rest], Y, MX, MapIn) ->
  Fold =
    fun(S, {X, Map}) ->
        NewMap =
          case S of
            $\n -> Map;
            _ -> Map#{{Y, X} => S}
          end,
        {X + 1, NewMap}
    end,
  {X, NewMap} =
    lists:foldl(Fold, {0, MapIn}, Line),
  make_map(Rest, Y + 1, max(MX, X), NewMap).

%% print({Map, MaxX, MaxY}) ->
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
%%         lists:foreach(For, lists:seq(0, MaxX)),
%%         io:format("~n")
%%     end,
%%   lists:foreach(Foreach, lists:seq(0, MaxY)).

evolve(N, Map) ->
  evolve(N, {erlang:phash2(Map), Map}, #{}).

evolve(0, {_, Map}, _) ->
  Map;
evolve(N, {Hash, Map}, Memo) ->
  {NewMap, NewMemo} =
    case maps:get(Hash, Memo, no) of
      no ->
        NM = evolve(Map),
        NHash = erlang:phash2(NM),
        NMe = Memo#{Hash => {NHash, NM}},
        {{NHash, NM}, NMe};
      O ->
        {O, Memo}
    end,
  evolve(N - 1, NewMap, NewMemo).

evolve({Map, MaxX, MaxY}) ->
  RFold =
    fun(Y, RAcc) ->
        Fold =
          fun(X, Acc) ->
              W = {Y, X},
              C = maps:get(W, Map),
              {O, T, Ya} = count_neighs(W, {MaxY, MaxX}, Map),
              case {C, O, T, Ya} of
                {$., _, N, _} when N >= 3 -> Acc#{W => $|};
                {$|, _, _, N} when N >= 3 -> Acc#{W => $#};
                {$#, _, T, Ya} when Ya == 0; T == 0 -> Acc#{W => $.};
                _ -> Acc
              end
          end,
        lists:foldl(Fold, RAcc, lists:seq(0, MaxX))
    end,
  {lists:foldl(RFold, Map, lists:seq(0, MaxY)), MaxX, MaxY}.

count_neighs({Y, X}, {MaxY, MaxX}, Map) ->
  Ns =
    [{YY, XX}
     || YY <- lists:seq(Y - 1, Y + 1),
        XX <- lists:seq(X - 1, X + 1),
        YY >= 0,
        XX >= 0,
        YY =< MaxY,
        XX =< MaxX,
        {YY, XX} =/= {Y, X}],
  Fold =
    fun(W, {O, T, Ya}) ->
        case maps:get(W, Map) of
          $. -> {O + 1, T, Ya};
          $| -> {O, T + 1, Ya};
          $# -> {O, T, Ya + 1}
        end
    end,
  lists:foldl(Fold, {0, 0, 0}, Ns).

score({Map, _, _}) ->
  Fold =
    fun(_, V, {T, Y}) ->
        case V of
          $| -> {T + 1, Y};
          $# -> {T, Y + 1};
          _ -> {T, Y}
        end
    end,
  {Y, T} = maps:fold(Fold, {0, 0}, Map),
  Y * T.
