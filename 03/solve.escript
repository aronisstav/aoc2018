#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/3

main(Args) ->
  Input = read_list("#~d @ ~d,~d: ~dx~d"),
  {N, Cover} = overlap(Input),
  Answer =
    case Args of
      ["2"] -> uncontested(Input, Cover);
      _ -> io_lib:format("~p", [N])
    end,
  io:format("~s~n", [Answer]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

overlap(Input) ->
  overlap(Input, 0, #{}).

overlap([], N, Cover) ->
  {N, Cover};
overlap([[_Id, X, Y, DX, DY]|Rest], N, Cover) ->
  Coords =
    [{CX, CY} ||
      CX <- [X + I || I <- lists:seq(1, DX)],
      CY <- [Y + I || I <- lists:seq(1, DY)]],
  Fold =
    fun(Coord, {NAcc, CoverAcc}) ->
        case maps:get(Coord, CoverAcc, none) of
          none -> {NAcc, CoverAcc#{Coord => cover}};
          cover -> {NAcc + 1, CoverAcc#{Coord => overlap}};
          overlap -> {NAcc, CoverAcc}
        end
    end,
  {NewN, NewCover} = lists:foldl(Fold, {N, Cover}, Coords),
  overlap(Rest, NewN, NewCover).

uncontested([[Id, X, Y, DX, DY]|Rest], Cover) ->
  Coords =
    [{CX, CY} ||
      CX <- [X + I || I <- lists:seq(1, DX)],
      CY <- [Y + I || I <- lists:seq(1, DY)]],
  Pred = fun(Coord) -> maps:get(Coord, Cover) =:= cover end,
  case lists:all(Pred, Coords) of
    true -> io_lib:format("~p", [Id]);
    false -> uncontested(Rest, Cover)
  end.
