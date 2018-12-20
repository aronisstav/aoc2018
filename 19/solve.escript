#!/usr/bin/env escript
-mode(native).
%-mode(compile).

%% https://adventofcode.com/2018/day/19

main(Args) ->
  Input = read_program(),
  Answer =
    case Args of
      ["2"] ->
        execute(Input);
        %%execute(1, Input);
      _ ->
        execute(0, Input)
    end,
  io:format("~p~n", [Answer]).

read_program() ->
  {ok, [IPR]} = io:fread("", "#ip ~d"),
  {IPR, read_program(0, #{})}.

read_program(N, Acc) ->
  case io:fread("", "~s ~d ~d ~d") of
    {ok, [Op, RA, RB, RC]} ->
      read_program(N + 1, Acc#{N => {Op, RA, RB, RC}});
    eof ->
      Acc
  end.

execute({IPR, Program}) ->
  OpMap = maps:from_list(ops()),
  execute(1, Program, IPR, planted(), OpMap).

execute(Init0, {IPR, Program}) ->
  OpMap = maps:from_list(ops()),
  InitMem = fresh(),
  execute(1, Program, IPR, InitMem#{0 => Init0}, OpMap).

execute(C, Program, IPR, Mem, OpMap) ->
  IP = maps:get(IPR, Mem),
  case maps:get(IP, Program, oom) of
    oom ->
      io:format("~p ~w~n", [C, [M || {_, M} <- maps:to_list(Mem)]]),
      maps:get(0, Mem);
    {Op, RA, RB, RC} = _PP ->
      Fun = (maps:get(Op, OpMap))(RA, RB, RC),
      OpMem = Fun(Mem),
      PreNIP = maps:get(IPR, OpMem),
      NewMem = OpMem#{IPR => PreNIP + 1},
      case C > 6297213 andalso C < 6297813 of
        true ->
          io:format("~p ~w~n", [C, [M || {_, M} <- maps:to_list(Mem)]]);
        false -> ok
      end,
      %io:format("~w ~p ~w~n", [Mem, _PP, NewMem]),
      execute(C + 1, Program, IPR, NewMem, OpMap)
  end.

fresh() ->
  #{0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0}.

planted() ->
  #{ 0 => 1
   , 1 => 0
   , 2 => 10551287
   , 3 => 10551286
   , 4 => 5
   , 5 => 10551286
   }.

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
