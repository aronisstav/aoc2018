#!/usr/bin/env escript
-mode(native).

%% https://adventofcode.com/2018/day/7

main(Args) ->
  Input = read_list("Step ~s must be finished before step ~s can begin."),
  {Graph, Nodes} = make_graph(Input),
  Sol =
    case Args of
      ["2"] -> "TODO";
      _ -> alphorder(Graph, Nodes)
    end,
  io:format("~s~n", [Sol]).

read_list(Pat) ->
  read_list(Pat, []).

read_list(Pat, Acc) ->
  case io:fread("", Pat) of
    {ok, Res} -> read_list(Pat, [Res|Acc]);
    eof -> lists:reverse(Acc)
  end.

alphorder(Graph, Nodes) ->
  Order = alphatopo(Graph, Nodes),
  io_lib:format("~s", [Order]).

make_graph(Input) ->
  make_graph(Input, #{}, #{}, []).

make_graph([], GraphOut, GraphIn, Nodes) ->
  EmptyNodeMap = maps:from_list([{N, []} || N <- Nodes]),
  FullGraphOut = maps:merge(EmptyNodeMap, GraphOut),
  FullGraphIn = maps:merge(EmptyNodeMap, GraphIn),
  {{FullGraphOut, FullGraphIn}, Nodes};
make_graph([[[From], [To]]|Rest], GraphOut, GraphIn, Nodes) ->
  Add = fun(El) -> fun(Set) -> ordsets:add_element(El, Set) end end,
  NewGraphOut = maps:update_with(From, Add(To), [To], GraphOut),
  NewGraphIn = maps:update_with(To, Add(From), [From], GraphIn),
  NewNodes = ordsets:add_element(From, ordsets:add_element(To, Nodes)),
  make_graph(Rest, NewGraphOut, NewGraphIn, NewNodes).

alphatopo(Graph, Nodes) ->
  alphatopo(Graph, Nodes, []).

alphatopo(_Graph, [], Sort) ->
  lists:reverse(Sort);
alphatopo({GraphOut, GraphIn}, Nodes, Sort) ->
  IsEmpty =
    fun(K, V, Acc) ->
        case V =:= [] of
          true -> [K|Acc];
          false -> Acc
        end
    end,
  [N|_] = lists:sort(maps:fold(IsEmpty, [], GraphIn)),
  NewGraphOut = delete_node(N, GraphOut),
  NewGraphIn = delete_node(N, GraphIn),
  NewNodes = Nodes -- [N],
  alphatopo({NewGraphOut, NewGraphIn}, NewNodes, [N|Sort]).

delete_node(N, Graph) ->
  Remove =
    fun(K, _V, Acc) when K =:= N -> Acc;
       (K, V, Acc)  -> Acc#{K => ordsets:del_element(N, V)}
    end,
  maps:fold(Remove, #{}, Graph).
