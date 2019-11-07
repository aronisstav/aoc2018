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
  best(Input, 23427187, 10663267, 9367153, 918, 43457607).

best(Input, X, Y, Z, B, D) ->
  Map =
    fun({XX, YY, ZZ}) ->
        Fold =
          fun([X, Y, Z, R], N) ->
              case manhattan([X, Y, Z], [XX, YY, ZZ]) =< R of
                true -> N + 1;
                false -> N
              end
          end,
        N = lists:foldl(Fold, 0, Input),
        ND = manhattan([0,0,0], [XX,YY,ZZ]),
        {ND, N, {XX, YY, ZZ}}
    end,
  Mapped =
    [Map({XX, YY, ZZ})
     || XX <- lists:seq(X - 1, X),
        YY <- lists:seq(Y - 1, Y),
        ZZ <- lists:seq(Z - 1, Z)],
  Good = lists:sort([X || X = {_, N, _} <- Mapped, N =:= B]),
  %io:format("~p~n", [{Mapped, Good}]),
  case Good of
    [{ND, _, {XX, YY, ZZ}} = Q|_] when ND < D ->
      %io:format("~p~n", [Q]),
      best(Input, XX, YY, ZZ, B, ND);
    _ ->
      D
  end.
