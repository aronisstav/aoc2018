#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/12

main(Args) ->
  {ok, [Initial]} = io:fread("", "initial state: ~s"),
  {ok, []} = io:fread("", ""),
  Evolve = make_evolve(read_list("~c~c~c~c~c => ~c")),
  Sol =
    case Args of
      ["2"] ->  sum_state(state_after(50 * 1000, {0, Initial}, Evolve));
      _ -> sum_state(state_after(20, {0, Initial}, Evolve))
    end,
  io:format("~p~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

make_evolve(List) ->
  Fold =
    fun([[A],[B],[C],[D],[E],[F]], Map) ->
        Map#{[A,B,C,D,E] => F}
    end,
  lists:foldl(Fold, #{}, List).

state_after(0, State, _Evolve) ->
  State;
state_after(N, State, Evolve) ->
  %io:format("~p~n", [State]),
  NewState = evolve(State, Evolve),
  state_after(N - 1, NewState, Evolve).

evolve({F, Line}, Evolve) ->
  evolve("...." ++ Line ++ "....", Evolve, F-2, []).

evolve([A,B,C,D,E|Rest], Evolve, F, Acc) ->
  NC = maps:get([A,B,C,D,E], Evolve, $.),
  {NF, NAcc} =
    case Acc =:= [] andalso NC =:= $. of
      true -> {F + 1, []};
      false -> {F, [NC|Acc]}
    end,
  evolve([B,C,D,E|Rest], Evolve, NF, NAcc);
evolve(_, _, F, Acc) ->
  Pred = fun(C) -> C =:= $. end,
  {F, lists:reverse(lists:dropwhile(Pred, Acc))}.

sum_state({F, List}) ->
  sum_state(F, List, 0).

sum_state(_, [], Sum) -> Sum;
sum_state(I, [C|Rest], Sum) ->
  NSum =
    case C =:= $# of
      true -> Sum + I;
      false -> Sum
    end,
  sum_state(I+1, Rest, NSum).
