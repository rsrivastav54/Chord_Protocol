-module(nodeCreation).

%% API
-export([spawnProcess/3, init/1, handle_cast/2, handle_call/3, getSuccessor/4, checkForFailedPred/1]).
-import(stabilizeChordRing, [beginStabilizer/3]).
-import(utilityFunctions, [checkRangeM/4, findSuccessor/1, findElement/2, findPid/1]).
-behavior(gen_server).

spawnProcess(NodeHash, M, CurrNodes) ->
  gen_server:start_link({global, list_to_atom(string:concat("Node", integer_to_list(NodeHash)))},?MODULE, [NodeHash, M, CurrNodes], []).

notify(NodeId, State) ->
  Predecessor = maps:get("predecessor", State),
  Id = maps:get("id", State),
  M = maps:get("m", State),
  IsValidRange = checkRangeM(Predecessor, NodeId, Id, M),
  if ((Predecessor =:= -1) or (IsValidRange =:= true)) ->
    ets:insert(table, {Id, NodeId}),
    NewState = maps:update("predecessor", NodeId, State);
    true -> NewState = State
  end,
  NewState.

sendCompletion(Id, HopsCount, Key, Succ) ->
  global:sync(),
  Pid = global:whereis_name(requestCompletedTask),
  Pid ! {completed, Id, HopsCount, Key, Succ}.

lookupSuccessor(Origin, NodeId, HopsCount, State) ->
  Successor = findSuccessor(State),
  Id = maps:get("id", State),
  M = maps:get("m", State),
  IsValidRange = checkRangeM(Id, NodeId, Successor + 1, M),
  if  IsValidRange =:= true ->
    sendCompletion(Origin, HopsCount, NodeId, Successor);
    true ->
      ClosestNode = closestTo(NodeId, State),
      lookupOfClosest(ClosestNode, Id, NodeId, HopsCount + 1, Origin)
  end.

closestTo(NodeId, State) ->
  M = maps:get("m", State),
  FingerTable = maps:get("fingerTable", State),
  Id = maps:get("id", State),
  Range = lists:seq(M - 1, 0, -1),
  Table = lists:map(fun(I) -> findElement(FingerTable, I) end, Range),
  Entry = findElementInRange(0, length(Table), Table, Id, NodeId, M),
  if Entry =:= not_found -> Id;
    true -> Entry
  end.

findElementInRange(End, End, _, _, _, _) -> not_found;
findElementInRange(Start, End, List, Id, NodeId, M) ->
  Element = findElement(List, Start),
  IsValidRange = checkRangeM(Id, Element, NodeId, M),
  if IsValidRange =:= true ->
    Element;
    true -> findElementInRange(Start + 1, End, List, Id, NodeId, M)
  end.

lookupOfClosest(Closest, Id, NodeId, HopsCount, Origin) ->
  if ((Closest =:= Id) or (Origin =:= Closest)) ->
    sendCompletion(Origin, HopsCount, NodeId, Closest);
    true ->
      Pid = findPid(Closest),
      gen_server:cast(Pid, {lookup, Origin, NodeId, HopsCount})
  end.

getSuccessor(Origin, NodeId, HopsCount, State) ->
  Id = maps:get("id", State),
  M = maps:get("m", State),
  Successor = findSuccessor(State),
  IsValidRange = checkRangeM(Id, NodeId, Successor + 1, M),
  if (IsValidRange =:= true) -> [Successor, HopsCount];
    true ->
      ClosestNode = closestTo(NodeId, State),
      getNearestSuccessor(ClosestNode, Id, NodeId, HopsCount + 1, Origin)
  end.

getNearestSuccessor(Closest, Id, NodeId, HopsCount, Origin) ->
  if ((Closest =:= Id) or (Origin =:= Closest)) -> [Id, HopsCount];
    true ->
      Pid = findPid(Closest),
      gen_server:call(Pid, {findSuccessor, Origin, NodeId, HopsCount})
  end.

checkForFailedPred(State) ->
  Predecessor = maps:get("predecessor", State),
  if Predecessor =/= -1 -> true;
    true -> false
  end.

init(Args) ->
  [NodeHash, M, CurrNodes] = Args,
  if length(CurrNodes) =:= 0 ->
    RandomNode = NodeHash;
    true ->
      RandomNode = lists:nth(rand:uniform(length(CurrNodes)), CurrNodes)
  end,
  FingerTable = lists:duplicate(M, RandomNode),
  State = maps:new(),
  StateWithId = maps:put("id", NodeHash, State),
  StateWithPre = maps:put("predecessor", -1, StateWithId),
  StateWithFinger = maps:put("fingerTable", FingerTable, StateWithPre),
  StateWithSuc = maps:put("next", 0, StateWithFinger),
  FinalState = maps:put("m", M, StateWithSuc),
  {ok, FinalState}.

handle_cast({startStabilize, NodeList, Node}, State) ->
  NewState = beginStabilizer(State, NodeList, Node),
  {noreply, NewState};
handle_cast({notify, NodeId}, State) ->
  NewState = notify(NodeId, State),
  {noreply, NewState};
handle_cast({lookup, Origin, Key, HopsCount}, State) ->
  Id = maps:get("id", State),
  if Key =:= Id ->
    sendCompletion(Origin, HopsCount, Key, Key);
    true -> lookupSuccessor(Origin, Key, HopsCount, State)
  end,
  {noreply, State}.

handle_call({findSuccessor, Origin, NodeId, HopsCount}, _From, State) ->
  {reply, getSuccessor(Origin, NodeId, HopsCount, State), State};
handle_call({state}, _From, State) ->
  {reply, State, State}.
