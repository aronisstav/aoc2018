#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/1

main(Args) ->
  Nums = lists:append(read_list("~d")),
  Sol =
    case Args of
      ["2"] -> find_ans(Nums);
      _ -> lists:sum(Nums)
    end,
  io:format("~w~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

find_ans([_|_] = Nums) ->
  find_ans(0, Nums, Nums, sets:new()).

find_ans(CF, [], Nums, Found) ->
  find_ans(CF, Nums, Nums, Found);
find_ans(CF, [C|R], Nums, Found) ->
  case sets:is_element(CF, Found) of
    true -> CF;
    false ->
      find_ans(CF + C, R, Nums, sets:add_element(CF, Found))
  end.
