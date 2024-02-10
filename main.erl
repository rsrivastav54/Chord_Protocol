% Authors : Rishabh Srivastav (UF ID : 76599488), Ashish Sunny Abraham (UF ID : 63887782)

-module(main).

%% API
-export([start/0]).
-import(chordRing, [start/1]).

% Program execution begins here
start() ->
  {_, [N]} = io:fread("Enter number of Nodes: ", "~d"),
  {_, [M]} = io:fread("Enter number of message requests: ", "~d"),
  {_, [Error]} = io:fread("Enter the number of failure nodes: ", "~d"),
  E = round(math:ceil(math:log2(N))),
  start({N,M,E, Error}).