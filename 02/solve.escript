#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/2

main(Args) ->
  Input = lists:append(read_list("~s")),
  Answer =
    case Args of
      ["2"] -> diffone(Input);
      _ -> checksum(Input)
    end,
  io:format("~s~n", [Answer]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

checksum(Strings) ->
  checksum(Strings, #{2 => 0, 3 => 0}).

checksum([], #{2 := M, 3 := N}) ->
  io_lib:format("~w", [M * N]);
checksum([String|Rest], #{2 := M, 3 := N}) ->
  {Add2, Add3} = twos_threes(String),
  checksum(Rest, #{2 => M + Add2, 3 => N + Add3}).

%% Pattern match on sorted list of characters
twos_threes(String) ->
  Sort = lists:sort(String),
  twos_threes(Sort, 0, 0).

twos_threes([], Add2, Add3) -> {Add2, Add3};
twos_threes([X,X,X|R], Add2, _Add3) ->
  twos_threes([C || C <- R, C =/= X], Add2, 1);
twos_threes([X,X|R], _Add2, Add3) ->
  twos_threes(R, 1, Add3);
twos_threes([_|R], Add2, Add3) ->
  twos_threes(R, Add2, Add3).

%% Generate candidates from each string by replacing one char with a
%% 0 and store them in a map to detect duplicates.
diffone(Strings) ->
  diffone(Strings, #{}).

diffone([], _) -> io_lib:format("XXX FAIL XXX");
diffone([String|Rest], Found) ->
  Cands = make_cands(String),
  Fold =
    fun(_Cand, {true, _} = R) -> R;
       (Cand, {false, FoundAcc}) ->
        case maps:get(Cand, FoundAcc, false) of
          false ->
            {false, FoundAcc#{Cand => true}};
          true ->
            {true, Cand}
        end
    end,
  case lists:foldl(Fold, {false, Found}, Cands) of
    {false, NewFound} ->
      diffone(Rest, NewFound);
    {true, Hit} ->
      Hit -- [0]
  end.

make_cands(String) ->
  Map =
    fun(I) ->
        {P, [_|S]} = lists:split(I, String),
        P ++ [0|S]
    end,
  [Map(I) || I <- lists:seq(0, length(String) - 1)].
