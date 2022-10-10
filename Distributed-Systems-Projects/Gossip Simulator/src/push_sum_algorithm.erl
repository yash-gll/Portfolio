-module(push_sum_algorithm).
-export([start/4]).
%-export([pass_message_to_neighbours/5]).
-import(topology, [neighbor_list/4]).

start(Index, Node_count, Topology, Master) ->
  io:fwrite("\nStarted push-sum node with index: ~p",[Index]),
  io:fwrite("\nThe Topology used is: ~p",[Topology]),
  io:fwrite("\nTotal number of nodes: ~p",[Node_count]),
  io:fwrite("\nThe Master is at: ~p",[Master]).

