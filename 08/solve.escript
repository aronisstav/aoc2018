#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/8

main(Args) ->
  [Input] = read_input(),
  Sol =
    case Args of
      ["2"] -> value([Input], [1]);
      _ -> sum_metadata([Input])
    end,
  io:format("~p~n", [Sol]).

read_input() ->
  read_input(1).

read_input(0) -> [];
read_input(Nodes) ->
  {ok, [ChildrenCount, MetaCount]} = io:fread("", "~d ~d"),
  Children = read_input(ChildrenCount),
  ReadMeta =
    fun() ->
        {ok, [M]} = io:fread("", "~d"),
        M
    end,
  Meta = [ReadMeta() || _ <- lists:seq(1, MetaCount)],
  Rest = read_input(Nodes - 1),
  [{Children, Meta} | Rest].

sum_metadata([]) -> 0;
sum_metadata([{Children, Meta}|Rest]) ->
  A = sum_metadata(Rest),
  B = sum_metadata(Children),
  C = lists:sum(Meta),
  A + B + C.

value([], Meta) ->
  lists:sum(Meta);
value(Children, Which) ->
  Marked = select(Which, Children),
  lists:sum([value(C, M) || {C, M} <- Marked]).

select(Which, Children) ->
  select(Which, Children, []).

select([], _, Acc) ->
  Acc;
select([N|R], Children, Acc) ->
  try
    C = lists:nth(N, Children),
    select(R, Children, [C|Acc])
  catch
    _:_ ->
      select(R, Children, Acc)
  end.
