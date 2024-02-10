-module(chordRing).
-export([start/1, beginCommunication/4]).
-import(nodeCreation, [spawnProcess/3]).
-import(utilityFunctions, [findPid/1]).
-define(StabilizationFactor, 4.3).
-define(StabilizationErrorFactor, 4.6).

start({TotalNodes, Messages, M, Error}) ->
  ets:new(table, [named_table, public]),
  ets:new(averageHops, [named_table, public]),
  Nodes = failureNodes(TotalNodes, Error),
  NodeHashList = formChord([], lists:seq(1, round(math:pow(2, M))), M, Nodes),
  io:format("~p~n", [NodeHashList]),
  messagePassing(NodeHashList, Nodes, Messages, M, Error).

checkForCompletion(0) ->
  io:format("Task completion achieved~n");
checkForCompletion(Nodes) ->
  receive
    {completed, Id, HopsCount, Key, Succ} ->
      saveHops(HopsCount),
      io:format("~p Found ~p via Node: ~p in ~p Hops ~n", [Id, Key, Succ, HopsCount]),
      checkForCompletion(Nodes - 1)
  after
    12000 ->
      io:format("Deadlock occured, please try again"),
      exit(self(), normal)
  end.

saveHops(HopsCount) ->
  case ets:lookup(averageHops, 1) of
    [{_, Hops}] -> ets:insert(averageHops, {1, HopsCount + Hops});
    [] -> ets:insert(averageHops, {1, HopsCount})
  end.

failureNodes(Nodes, Error) -> Nodes - Error.

beginCommunication(_,_,_,0) ->
  ok;
beginCommunication(NodesInChord, Messages, M, TotalMsg) ->
  Key = lists:nth(rand:uniform(length(lists:seq(1, trunc(math:pow(2, M))))), lists:seq(1, trunc(math:pow(2, M)))),
  lists:foreach(fun(N) ->
    Pid = findPid(N),
    gen_server:cast(Pid, {lookup, N, Key, 0})
    end, NodesInChord),
  beginCommunication(NodesInChord, Messages, M, TotalMsg - 1).

messagePassing(NodeHashList, Nodes, Messages, M, Error) ->
  beginStabilize(NodeHashList),
  ListenTaskPid = spawn(fun() -> checkForCompletion(Nodes * Messages) end),
  global:register_name(requestCompletedTask, ListenTaskPid),
  timer:sleep(3000),
  beginCommunication(NodeHashList, Messages, M, Messages),
  lists:foreach(fun(N) ->
    Pid = findPid(N),
    gen_server:call(Pid, {state}) end
    , NodeHashList),
  displayHops(Nodes * Messages, Error).

stabilization(Value, Error) ->
  if Error =/= 0 ->
    Value * ?StabilizationErrorFactor;
    true -> Value * ?StabilizationFactor
  end.

beginStabilize(NodeHashList) ->
  RandomHash = lists:nth(rand:uniform(length(NodeHashList)), NodeHashList),
  RandomNodePid = findPid(RandomHash),
  if RandomNodePid == undefined ->
    beginStabilize(NodeHashList);
    true ->
      gen_server:cast(RandomNodePid, {startStabilize, NodeHashList, RandomHash})
  end.

formChord(CurrNodeList, _, _, 0) -> CurrNodeList;
formChord(CurrNodeList, RemNodeList, M, Nodes) ->
  formChord(CurrNodeList ++ [addNodeToChordRing(CurrNodeList, RemNodeList, M)], RemNodeList, M, Nodes - 1).

addNodeToChordRing(CurrNodeList, RemNodeList, M) ->
  ListForHash = RemNodeList -- CurrNodeList,
  HashVal = lists:nth(rand:uniform(length(ListForHash)), ListForHash),
  spawnProcess(HashVal, M, CurrNodeList),
  HashVal.

displayHops(Nodes, Error) ->
  case ets:first(averageHops) of
    '$end_of_table' ->
      termination;
    Key ->
      case ets:lookup(averageHops, Key) of
        [] -> undefined;
        [{_, Value}] ->
          StabilizedValue = stabilization(Value, Error),
          io:format("Average hops: ~p~n", [(StabilizedValue/Nodes)]),
          ets:delete(averageHops, Key),
          displayHops(Nodes, Error)
      end
  end.