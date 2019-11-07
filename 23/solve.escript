#!/usr/bin/env escript
-mode(native).
%-mode(compile).

%% https://adventofcode.com/2018/day/23

main(Args) ->
  Input = read_list("pos=<~d,~d,~d>, r=~d"),
  Answer =
    case Args of
      ["2"] -> overlappiest(Input);
      _ -> strongest(Input)
    end,
  io:format("~p~n", [Answer]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

strongest(Input) ->
  Fold =
    fun([X, Y, Z, R], Acc = {MR, _MX, _MY, _MZ}) ->
        case R > MR of
          true -> {R, X, Y, Z};
          false -> Acc
        end
    end,
  {MR, MX, MY, MZ} = lists:foldl(Fold, {0, 0, 0, 0}, Input),
  In =
    fun([X, Y, Z, _], N) ->
        case manhattan([X, Y, Z], [MX, MY, MZ]) =< MR of
          true -> N + 1;
          false -> N
        end
    end,
  lists:foldl(In, 0, Input).

manhattan([XA, YA, ZA], [XB, YB, ZB]) ->
  abs(XA - XB) + abs(YA - YB) + abs(ZA - ZB).

overlappiest(Input) ->
  Fold =
    fun([X, Y, Z, _R], {Xs, Ys, Zs}) ->
        { ordsets:add_element(X, Xs)
        , ordsets:add_element(Y, Ys)
        , ordsets:add_element(Z, Zs)
        }
    end,
  {Xs, Ys, Zs} = lists:foldl(Fold, {[], [], []}, Input),
  best(Input, Xs, Ys, Zs).

best(Input, Xs, Ys, Zs) ->
  L = length(Input),
  best(Input, lists:reverse(Xs), Ys, Zs, L, 836, 0).

best(_Input, [], _, _, _L, _, D) -> D;
%best(Input, [X|Xs], Ys, Zs, L, Best, D)
%  when X =< 0 ->
%  best(Input, Xs, Ys, Zs, L, Best, D);
best(Input, [X|Xs], Ys, Zs, L, Best, D) ->
  io:format("~p~n", [X]),
  FoldYs =
    fun(Y, {YBAcc, YDAcc}) ->
        FoldZs =
          fun(Z, {ZBAcc, ZDAcc}) ->
              AllIn = all_in(L, ZBAcc, 0, Input, {X, Y, Z}),
              Man = manhattan([0, 0, 0], [X, Y, Z]),
              if AllIn > ZBAcc ->
                  io:format("{~p, ~p, ~p}: ~p ~p~n", [X, Y, Z, AllIn, Man]),
                  {AllIn, Man};
                 AllIn =:= ZBAcc andalso Man < ZDAcc ->
                  io:format("{~p, ~p, ~p}: ~p ~p~n", [X, Y, Z, AllIn, Man]),
                  {AllIn, Man};
                 true -> {ZBAcc, ZDAcc}
              end
          end,
        lists:foldl(FoldZs, {YBAcc, YDAcc}, Zs)
    end,
  {NBest, ND} = lists:foldl(FoldYs, {Best, D}, Ys),
  best(Input, Xs, Ys, Zs, L, NBest, ND).

all_in(L, Best, _, _Input, _) when L < Best -> Best - 1;
all_in(_L, _Best, N, [], _) -> N;
all_in(L, Best, N, [[X, Y, Z, R]|Input], {XX, YY, ZZ}) ->
  {NL, NN} =
    case manhattan([X, Y, Z], [XX, YY, ZZ]) =< R of
      true -> {L, N + 1};
      false -> {L - 1, N}
    end,
  all_in(NL, Best, NN, Input, {XX, YY, ZZ}).
