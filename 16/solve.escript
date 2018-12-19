#!/usr/bin/env escript
%-mode(native).
-mode(compile).

%% https://adventofcode.com/2018/day/16

main(Args) ->
  Answer =
    case Args of
      ["2"] ->
        Program = read_program(),
        execute(Program);
      _ ->
        Input = read_samples(),
        {M, D} = analyze(Input),
        MM = solve(M),
        {MM, D}
    end,
  io:format("~p~n", [Answer]).

read_samples() ->
  read_samples([]).

read_samples(Acc) ->
  case io:fread("", "Before: [~d,~d,~d,~d]") of
    {ok, Before} ->
      {ok, Ops} = io:fread("", "~d ~d ~d ~d"),
      {ok, After} = io:fread("", "After: [~d,~d,~d,~d]"),
      read_samples([{Before, Ops, After}|Acc]);
    eof -> Acc
  end.

analyze(Input) ->
  Ops = ops(),
  Fold =
    fun({[BR0, BR1, BR2, BR3], [Op, RA, RB, RC], [AR0, AR1, AR2, AR3]}, {Map, N}) ->
        Before = #{0 => BR0, 1 => BR1, 2 => BR2, 3 => BR3},
        After = #{0 => AR0, 1 => AR1, 2 => AR2, 3 => AR3},
        FoldOps =
          fun({Name, OpFun}, Acc) ->
              case (OpFun(RA, RB, RC))(Before) =:= After of
                true -> [Name|Acc];
                false -> Acc
              end
          end,
        Matching = lists:foldl(FoldOps, [], Ops),
        NN =
          case length(Matching) >= 3 of
            true -> N + 1;
            false -> N
          end,
        Impos = all() -- Matching,
        #{Op := Old} = Map,
        NMap = Map#{Op => Old -- Impos},
        {NMap, NN}
    end,
  lists:foldl(Fold, {unknown(), 0}, Input).

all() ->
  [N || {N, _} <- ops()].

unknown() ->
  maps:from_list([{N, all()} || N <- lists:seq(0, 15)]).

solve(Map) ->
  solve(Map, #{}).

solve(Map, Done) ->
  Find =
    fun(_, _, {true, _} = T) -> T;
       (K, V, A) ->
        case V of
          [One] -> {true, {K, One}};
          _ -> A
        end
    end,
  case maps:fold(Find, false, Map) of
    {true, {K, One}} ->
      Remove =
        fun(KK, VV, Acc) ->
            case K =:= KK of
              true -> Acc;
              false ->
                Acc#{KK => VV -- [One]}
            end
        end,
      NMap = maps:fold(Remove, #{}, Map),
      NDone = Done#{K => One},
      solve(NMap, NDone);
    false -> {Map, Done}
  end.

ops() ->
  [ { "addr"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = maps:get(RB, RM),
              RM#{ RC => AV + BV}
          end
      end
    }
  , { "addi"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = RB,
              RM#{ RC => AV + BV}
          end
      end
    }
  , { "mulr"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = maps:get(RB, RM),
              RM#{ RC => AV * BV}
          end
      end
    }
  , { "muli"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = RB,
              RM#{ RC => AV * BV}
          end
      end
    }
  , { "banr"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = maps:get(RB, RM),
              RM#{ RC => AV band BV}
          end
      end
    }
  , { "bani"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = RB,
              RM#{ RC => AV band BV}
          end
      end
    }
  , { "borr"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = maps:get(RB, RM),
              RM#{ RC => AV bor BV}
          end
      end
    }
  , { "bori"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = RB,
              RM#{ RC => AV bor BV}
          end
      end
    }
  , { "setr"
    , fun(RA, _RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              RM#{ RC => AV }
          end
      end
    }
  , { "seti"
    , fun(RA, _RB, RC) ->
          fun(RM) ->
              AV = RA,
              RM#{ RC => AV }
          end
      end
    }
  , { "gtrr"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = maps:get(RB, RM),
              CV = if AV > BV -> 1; true -> 0 end,
              RM#{ RC => CV}
          end
      end
    }
  , { "gtir"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = RA,
              BV = maps:get(RB, RM),
              CV = if AV > BV -> 1; true -> 0 end,
              RM#{ RC => CV}
          end
      end
    }
  , { "gtri"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = RB,
              CV = if AV > BV -> 1; true -> 0 end,
              RM#{ RC => CV}
          end
      end
    }
  , { "eqrr"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = maps:get(RB, RM),
              CV = if AV == BV -> 1; true -> 0 end,
              RM#{ RC => CV}
          end
      end
    }
  , { "eqir"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = RA,
              BV = maps:get(RB, RM),
              CV = if AV == BV -> 1; true -> 0 end,
              RM#{ RC => CV}
          end
      end
    }
  , { "eqri"
    , fun(RA, RB, RC) ->
          fun(RM) ->
              AV = maps:get(RA, RM),
              BV = RB,
              CV = if AV == BV -> 1; true -> 0 end,
              RM#{ RC => CV}
          end
      end
    }
  ].

read_program() ->
  read_program([]).

read_program(Acc) ->
  case io:fread("", "~d ~d ~d ~d") of
    {ok, [Op, RA, RB, RC]} ->
      read_program([{Op, RA, RB, RC}|Acc]);
    eof ->
      lists:reverse(Acc)
  end.

execute(Program) ->
  FunMap = maps:from_list(ops()),
  Fold =
    fun(K, V, Acc) ->
        Acc#{K => maps:get(V, FunMap)}
    end,
  OpMap = maps:fold(Fold, #{}, opmap()),
  execute(Program, fresh(), OpMap).

execute([], Mem, _) ->
  maps:get(0, Mem);
execute([{Op, RA, RB, RC}|R], Mem, OpMap) ->
  Fun = (maps:get(Op, OpMap))(RA, RB, RC),
  execute(R, Fun(Mem), OpMap).

fresh() ->
  #{0 => 0, 1 => 0, 2 => 0, 3 => 0}.

opmap() ->
  #{  0 => "eqri"
   ,  1 => "mulr"
   ,  2 => "gtri"
   ,  3 => "gtrr"
   ,  4 => "banr"
   ,  5 => "addi"
   ,  6 => "seti"
   ,  7 => "gtir"
   ,  8 => "muli"
   ,  9 => "bori"
   , 10 => "setr"
   , 11 => "addr"
   , 12 => "bani"
   , 13 => "borr"
   , 14 => "eqir"
   , 15 => "eqrr"
   }.
