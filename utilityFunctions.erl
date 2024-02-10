-module(utilityFunctions).

%% API
-export([checkRangeM/4, findElement/2, findSuccessor/1, findPid/1]).

checkRangeM(From, Key, To, M) ->
  PowInt = trunc(math:pow(2, M)),
  if To > PowInt + 1 ->
    NewTo = 1;
    true -> NewTo = To
  end,
  if From < NewTo ->
    Return = ((From < Key) and (Key < NewTo));
    From =:= NewTo ->
      Return = (Key =/= From);
    From > NewTo ->
      Return = (((Key > 0) and (Key < NewTo)) or ((Key > From) and (Key =< PowInt)))
  end,
  Return.

findElement(List, Index) ->
  {_, [Element|_]} = lists:split(Index, List),
  Element.

findSuccessor(State) ->
  findElement(maps:get("fingerTable", State), 0).

findPid(Node) ->
  global:sync(),
  Pid = global:whereis_name(list_to_atom(string:concat("Node", integer_to_list(Node)))),
  Pid.