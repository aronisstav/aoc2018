#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/7

-define(WORKERS, 5).
-define(BASE, 60).

main(Args) ->
  Input = read_list("Step ~s must be finished before step ~s can begin."),
  {Graph, Nodes} = make_graph(Input),
  Sol =
    case Args of
      ["2"] -> integer_to_list(timeproc(Graph, Nodes, ?WORKERS, ?BASE));
      _ -> alphatopo(Graph, Nodes)
    end,
  io:format("~s~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

make_graph(Input) ->
  make_graph(Input, #{}, #{}, []).

make_graph([], GraphOut, GraphIn, Nodes) ->
  EmptyNodeMap = maps:from_list([{N, []} || N <- Nodes]),
  FullGraphOut = maps:merge(EmptyNodeMap, GraphOut),
  FullGraphIn = maps:merge(EmptyNodeMap, GraphIn),
  {{FullGraphOut, FullGraphIn}, Nodes};
make_graph([[[FromC], [ToC]]|Rest], GraphOut, GraphIn, Nodes) ->
  From = FromC - $A + 1,
  To = ToC - $A + 1,
  Add = fun(El) -> fun(Set) -> ordsets:add_element(El, Set) end end,
  NewGraphOut = maps:update_with(From, Add(To), [To], GraphOut),
  NewGraphIn = maps:update_with(To, Add(From), [From], GraphIn),
  NewNodes = ordsets:add_element(From, ordsets:add_element(To, Nodes)),
  make_graph(Rest, NewGraphOut, NewGraphIn, NewNodes).

alphatopo(Graph, Nodes) ->
  alphatopo(Graph, Nodes, []).

alphatopo(_Graph, [], Sort) ->
  lists:reverse(Sort);
alphatopo(Graph, Nodes, Sort) ->
  [N|_] = lists:sort(roots(Graph)),
  NewGraph = delete_node(N, Graph),
  NewNodes = Nodes -- [N],
  alphatopo(NewGraph, NewNodes, [N - 1 + $A|Sort]).

roots({_, GraphIn}) ->
  IsEmpty =
    fun(K, V, Acc) ->
        case V =:= [] of
          true -> [K|Acc];
          false -> Acc
        end
    end,
  maps:fold(IsEmpty, [], GraphIn).

delete_node(N, {GraphOut, GraphIn}) ->
  Remove =
    fun(K, _V, Acc) when K =:= N -> Acc;
       (K, V, Acc)  -> Acc#{K => ordsets:del_element(N, V)}
    end,
  {maps:fold(Remove, #{}, GraphOut), maps:fold(Remove, #{}, GraphIn)}.

timeproc(Graph, Nodes, Workers, Base) ->

  timeproc(Nodes, [], [], Workers, Graph, Base, 0).

timeproc([], [], [], _Workers, _Graph, _Base, Time) ->
  Time;
timeproc(Nodes, Active, AssignedNodes, Workers, Graph, Base, Time) ->
  Roots = roots(Graph),
  FreeNodes = lists:sort(Roots -- AssignedNodes),
  case Workers =:= 0 orelse FreeNodes =:= [] of
    true ->
      [{T, _}|_] = Active,
      {DoneNodes, NewActive} = complete_smallest(T, Active),
      NewNodes = Nodes -- DoneNodes,
      NewAssignedNodes = AssignedNodes -- DoneNodes,
      NewWorkers = Workers + length(DoneNodes),
      RemDoneNodes =
        fun(N, GraphAcc) ->
            delete_node(N, GraphAcc)
        end,
      NewGraph = lists:foldl(RemDoneNodes, Graph, DoneNodes),
      timeproc(NewNodes, NewActive, NewAssignedNodes,
               NewWorkers, NewGraph, Base, Time + T);
    false ->
      [R|_] = FreeNodes,
      NewActive = ordsets:add_element({R + Base, R}, Active),
      NewAssignedNodes = ordsets:add_element(R, AssignedNodes),
      NewWorkers = Workers - 1,
      timeproc(Nodes, NewActive, NewAssignedNodes,
               NewWorkers, Graph, Base, Time)
  end.

complete_smallest(T, Active) ->
  complete_smallest(T, Active, [], []).

complete_smallest(_T, [], DoneAcc, ActiveAcc) ->
  {DoneAcc, lists:reverse(ActiveAcc)};
complete_smallest(T, [{Q, R}|Rest], DoneAcc, ActiveAcc) ->
  Rem = Q - T,
  case Rem =:= 0 of
    true ->
      complete_smallest(T, Rest, [R|DoneAcc], ActiveAcc);
    false ->
      complete_smallest(T, Rest, DoneAcc, [{Rem, R}|ActiveAcc])
  end.
