-module(stabilizeChordRing).

%% API
-export([beginStabilizer/3]).
-import(utilityFunctions, [checkRangeM/4, findSuccessor/1, findPid/1]).
-import(nodeCreation, [getSuccessor/4, checkForFailedPred/1]).

beginStabilizer(State, NodeList, NodeVal) ->
  Id = maps:get("id", State),
  State1 = stabilize(State),
  FinalState = fixFingerTable(State1, Id),
  NewM = maps:get("m", FinalState),
  RemNodeList = NodeList -- [NodeVal],
  if length(RemNodeList) =/= 0 ->
    startOnOtherNodes(NewM, RemNodeList);
    true -> ok
  end,
  FinalState.

findPredecessor(Id) ->
  case ets:lookup(table, Id) of
    [{_, P}] -> P;
    [] -> -1
  end.

stabilize(State) ->
  CurrSuccessor = findSuccessor(State),
  CurrId = maps:get("id", State),
  CurrPredecessor = maps:get("predecessor", State),
  M = maps:get("m", State),
  if CurrId =:= CurrSuccessor ->
    Predecessor = CurrPredecessor;
    true ->
      Predecessor = findPredecessor(CurrSuccessor)
  end,
  IsRangeValid = checkRangeM(CurrId, Predecessor, CurrSuccessor, M),
  if Predecessor =/= -1, IsRangeValid =:= true ->
    Successor = Predecessor;
    true ->
      Successor = CurrSuccessor
  end,
  informSuccessorForUpdation(Successor, CurrId),
  updateSuccessor(Successor, State, 0).

informSuccessorForUpdation(Successor, NewPredecessor) ->
  SuccessorPid = findPid(Successor),
  gen_server:cast(SuccessorPid, {notify, NewPredecessor}).

updateSuccessor(Successor, State, Index) ->
  FingerTable = maps:get("fingerTable", State),
  NewTable = lists:sublist(FingerTable,Index) ++ [Successor] ++ lists:nthtail(Index + 1,FingerTable),
  NewState = maps:update("fingerTable", NewTable, State),
  NewState.

fixFingerTable(State, Origin) ->
  Next = maps:get("next", State),
  M = maps:get("m", State),
  Id = maps:get("id", State),
  NumNodes = trunc(math:pow(2, M)),
  NextPossibleId = Id + trunc(math:pow(2, Next)),
  if NextPossibleId > NumNodes ->
    NextId = NextPossibleId - NumNodes;
    true -> NextId = NextPossibleId
  end,
  [Successor, _] = getSuccessor(Origin, NextId, 0, State),
  State1 = updateSuccessor(Successor, State, Next),
  FinalState = maps:update("next", (Next + 1) rem M , State1),
  FinalState.

startOnOtherNodes(M, RemNodes) ->
  RandomNode = lists:nth(rand:uniform(length(RemNodes)), RemNodes),
  Pid = findPid(RandomNode),
  if Pid =:= undefined ->
    startOnOtherNodes(M, RemNodes);
    true -> gen_server:cast(Pid, {startStabilize, RemNodes, RandomNode})
  end.
