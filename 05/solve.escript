#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/5

main(Args) ->
  {ok, [Input]} = io:fread("", "~s"),
  Answer =
    case Args of
      ["2"] -> best_length_after_react(Input);
      _ -> length_after_react(Input)
    end,
  io:format("~s~n", [Answer]).

length_after_react(Input) ->
  Stable = react(Input),
  io_lib:format("~p", [length(Stable)]).

react(Input) ->
  case react(Input, []) of
    false -> Input;
    {true, NewInput} -> react(NewInput)
  end.

react([_], _) -> false;
react([A|[B|C] = R], Acc) ->
  case abs(A - B) =:= abs($A - $a) of
    false ->
      react(R, [A|Acc]);
    true ->
      {true, lists:reverse(Acc, C)}
  end.

best_length_after_react(Input) ->
  Map =
    fun(Lower) ->
        Cleaned = remove_all(Lower, Input),
        length(react(Cleaned))
    end,
  Min = lists:min([Map(C) || C <- lists:seq($a, $z)]),
  io_lib:format("~p", [Min]).

remove_all(Lower, Input) ->
  remove_all(Lower, $A - $a + Lower, Input, []).

remove_all(_, _, [], Acc) -> lists:reverse(Acc);
remove_all(L, C, [A|R], Acc) ->
  case A =:= L orelse A =:= C of
    true -> remove_all(L, C, R, Acc);
    false -> remove_all(L, C, R, [A|Acc])
  end.
