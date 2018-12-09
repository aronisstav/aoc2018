#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/9

main(Args) ->
  {ok, [N, M]} = io:fread("", "~d ~d"),
  Sol =
    case Args of
      ["2"] -> high_score(N, M * 100);
      _ -> high_score(N, M)
    end,
  io:format("~p~n", [Sol]).

high_score(N, M) ->
  high_score({[0], []}, 1, #{}, N, M).

high_score(_Marbles, Turn, Scores, _N, Last) when Turn > Last ->
  FindWinner =
    fun(K, V, {_, MaxV} = Max) ->
        case V > MaxV of
          true -> {K, V};
          false -> Max
        end
    end,
  {_Winner, HighScore} = maps:fold(FindWinner, {0, 0}, Scores),
  HighScore;
high_score(Marbles, Turn, Scores, N, Last) ->
  case Turn rem 23 =:= 0 of
    false ->
      NewMarbles = add_rotate_2(Turn, Marbles),
      high_score(NewMarbles, Turn + 1, Scores, N, Last);
    true ->
      {H, NewMarbles} = rem_rotate_neg7(Marbles),
      Points = Turn + H,
      Update = fun(V) -> V + Points end,
      Winner = Turn rem N,
      NewScores = maps:update_with(Winner, Update, Points, Scores),
      high_score(NewMarbles, Turn + 1, NewScores, N, Last)
  end.

add_rotate_2(New, {[0], []}) -> {[New, 0], []};
add_rotate_2(New, {Front, Back}) ->
  case Front of
    [H1, H2|T] ->
      {[New|T], [H2, H1|Back]};
    [H1] ->
      [H2|T] = lists:reverse(Back),
      {[New|T], [H2, H1]};
    [] ->
      [H1, H2|T] = lists:reverse(Back),
      {[New|T], [H2, H1]}
  end.

rem_rotate_neg7({Front, Back}) ->
  case Back of
    [HN1, HN2, HN3, HN4, HN5, HN6, HN7|R] ->
      {HN7, {[HN6, HN5, HN4, HN3, HN2, HN1|Front], R}};
    _ ->
      Rev = Back ++ lists:reverse(Front),
      rem_rotate_neg7({[], Rev})
  end.
