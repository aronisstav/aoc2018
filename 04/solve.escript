#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/4

%% Sort the input first.

main(Args) ->
  Input = read_input(),
  Answer =
    case Args of
      ["2"] -> strategy2(Input);
      _ -> strategy1(Input)
    end,
  io:format("~s~n", [Answer]).

read_input() ->
  {ok, [_Y, _M, _D, _H, _Mi, Id]} =
    io:fread("", "[~d-~d-~d ~d:~d] Guard #~d begins shift"),
  read_input(Id, [], []).

read_input(Id, GAcc, Acc) ->
  case io:fread("", "[~d-~d-~d ~d:~d] ~s") of
    {ok, [_Y, _M, _D, _H, Mi, S]} ->
      case S of
        "falls" ->
          {ok, _} = io:fread("", " asleep"),
          {ok, [_, _, _, MiU]} = io:fread("", "[~d-~d-~d 00:~d] wakes up"),
          read_input(Id, [{Mi, MiU}|GAcc], Acc);
        "Guard" ->
          {ok, [NId]} = io:fread("", " #~d begins shift"),
          read_input(NId, [], [{Id, lists:reverse(GAcc)}|Acc])
      end;
    eof ->
      lists:reverse([{Id, lists:reverse(GAcc)}|Acc])
  end.

strategy1(Input) ->
  Sums = sum(Input),
  Guard = max_key(Sums),
  Likely = likely(fun(X) -> X =:= Guard end, fun(_Id, K) -> K end, Input),
  io_lib:format("~p", [Guard * Likely]).

sum(Input) ->
  sum(Input, #{}).

sum([], Map) ->
  Map;
sum([{Id, Times}|Rest], Map) ->
  Sum = lists:sum([E - S || {S, E} <- Times]),
  Update = fun(V) -> V + Sum end,
  sum(Rest, maps:update_with(Id, Update, Sum, Map)).

max_key(Map) ->
  Fold =
    fun(K, V, {Id, Max}) ->
        case V > Max of
          true -> {K, V};
          false -> {Id, Max}
        end
    end,
  {Id, _} = maps:fold(Fold, {0, 0}, Map),
  Id.

likely(Filter, Store, Input) ->
  likely(Filter, Store, Input, #{}).

likely(_Filter, _Store, [], Map) ->
  max_key(Map);
likely(Filter, Store, [{Id, Times}|Rest], Map) ->
  NewMap =
    case Filter(Id) of
      true ->
        Fold =
          fun({S, E}, MapAcc) ->
              Update =
                fun(K, MapAccAcc) ->
                    maps:update_with(
                      Store(Id, K),
                      fun(V) -> V + 1 end,
                      1,
                      MapAccAcc)
                end,
              Minutes = lists:seq(S, E - 1),
              lists:foldl(Update, MapAcc, Minutes)
          end,
        lists:foldl(Fold, Map, Times);
      false ->
        Map
    end,
  likely(Filter, Store, Rest, NewMap).

strategy2(Input) ->
  {Guard, Minute} =
    likely(fun(_) -> true end, fun(Id, K) -> {Id, K} end, Input),
  io_lib:format("~p", [Guard * Minute]).
