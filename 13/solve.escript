#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/13

main(Args) ->
  Lines = read_lines(),
  Map = make_map(Lines),
  Sol =
    case Args of
      ["2"] -> final_cart(Map);
      _ -> first_collision(Map)
    end,
  io:format("~p~n", [Sol]).

read_lines() ->
  read_lines([]).

read_lines(Acc) ->
  case io:get_line("") of
    eof -> lists:reverse(Acc);
    Line -> read_lines([Line|Acc])
  end.

make_map(Lines) ->
  make_map(Lines, 0, 0, #{}, []).

make_map([], Y, X, Map, Carts) ->
  {Carts, Map, Y - 1, X - 2};
make_map([Line|Rest], Y, MX, MapIn, CartsIn) ->
  Fold =
    fun(S, {X, Map, Carts}) ->
        NewMap =
          case S of
            H when H =:= $-; H =:= $<; H =:= $> -> Map#{{Y, X} => $-};
            V when V =:= $|; V =:= $^; V =:= $v -> Map#{{Y, X} => $|};
            $+ -> Map#{{Y, X} => S};
            $/ -> Map#{{Y, X} => S};
            $\\ -> Map#{{Y, X} => S};
            _ -> Map
          end,
        NewCarts =
          case S =:= $> orelse S =:= $< orelse S =:= $v orelse S =:= $^ of
            true -> orddict:store({Y, X}, {S, 1}, Carts);
            false -> Carts
          end,
        {X + 1, NewMap, NewCarts}
    end,
  {X, NewMap, NewCarts} = lists:foldl(Fold, {0, MapIn, CartsIn}, Line),
  make_map(Rest, Y + 1, max(MX, X), NewMap, NewCarts).

%% print({Carts, Map, MY, MX}) ->
%%   Foreach =
%%     fun(Y) ->
%%         For =
%%           fun(X) ->
%%               case orddict:find({Y, X}, Carts) of
%%                 {ok, {V, _}} ->
%%                   io:format("~c", [V]);
%%                 error ->
%%                   io:format("~c", [maps:get({Y,X}, Map, $ )])
%%               end
%%           end,
%%           lists:foreach(For, lists:seq(0, MX)),
%%           io:format("~n")
%%     end,
%%   lists:foreach(Foreach, lists:seq(0, MY)).

first_collision(Map) ->
  %% print(Map),
  %% io:format("~n"),
  case step(Map) of
    {[X|_], _NewMap} -> X;
    {[], NewMap} -> first_collision(NewMap)
  end.

step({Carts, Map, MY, MX}) ->
  {Crash, NewCarts} = step(Carts, Map, [], []),
  {Crash, {NewCarts, Map, MY, MX}}.

step([], _Map, NewCarts, Crashed) ->
  {lists:reverse(Crashed), NewCarts};
step([{{Y,X}, {Dir, C}}|Rest], Map, NCarts, Crashed) ->
  {NY, NX} =
    case Dir of
      $> -> {Y, X + 1};
      $< -> {Y, X - 1};
      $^ -> {Y - 1, X};
      $v -> {Y + 1, X}
    end,
  case {orddict:find({NY, NX}, Rest), orddict:find({NY, NX}, NCarts)} of
    {error, error} ->
      Track = maps:get({NY, NX}, Map),
      ND =
        case Track of
          $/ ->
            case Dir of
              $> -> $^;
              $< -> $v;
              $^ -> $>;
              $v -> $<
            end;
          $\\ ->
            case Dir of
              $> -> $v;
              $< -> $^;
              $^ -> $<;
              $v -> $>
            end;
          $+ ->
            case C of
              1 -> left(Dir);
              2 -> straight(Dir);
              3 -> right(Dir)
            end;
          _ -> Dir
        end,
      NC =
        case Track of
          $+ -> (C rem 3) + 1;
          _ -> C
        end,
      NewNCarts = orddict:store({NY, NX}, {ND, NC}, NCarts),
      step(Rest, Map, NewNCarts, Crashed);
    _ ->
      NewRest = orddict:erase({NY, NX}, Rest),
      NewNCarts = orddict:erase({NY, NX}, NCarts),
      step(NewRest, Map, NewNCarts, [{NX, NY}|Crashed])
  end.

straight(S) -> S.

left($>) -> $^;
left($<) -> $v;
left($^) -> $<;
left($v) -> $>.

right($>) -> $v;
right($<) -> $^;
right($^) -> $>;
right($v) -> $<.

final_cart(Map) ->
  %% print(Map),
  %% io:format("~n"),
  %% timer:sleep(500),
  case step(Map) of
    {_, {[{{Y,X},_}], _, _, _}} -> {X,Y};
    {_, NewMap} -> final_cart(NewMap)
  end.
