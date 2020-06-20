-module(ring).

%% API
-export([start/3, start_child/5]).

start(N, M, Msg) ->
  io:format("~s~p~n~n", ["SPAWNED PARENT @ ", self()]),
  CPid = spawn(ring, start_child, [self(), self(), N, M, Msg]),
  start_loop(CPid, M, Msg)
.

start_loop(CPid, M, Msg) ->
  CPid ! {self(), Msg},
  receive
    {_, _} when M =:= 0 -> ok;
    {From, Message}     ->
      io:format("~n~s~p~s~s~s~w~s~p~n", ["PARENT @ ", self(), " received ", Message, "#", M, " from", From]),
      start_loop(CPid, M-1, Msg)
  end
.


start_child(Op, Parent, 1, M, Msg) ->
  io:format("~s~w~s~p~s~p~n", ["\tSPAWNED CHILD #", 1, " @", self(), " FROM ", Parent]),
  start_child_loop(Op, M, Msg)
;
start_child(OP, Parent, N, M, Msg) ->
  io:format("~s~w~s~p~s~p~n", ["\tSPAWNED CHILD #", N, " @", self(), " FROM ", Parent]),
  CPid = spawn(?MODULE, start_child, [OP, self(), N-1, M, Msg]),
  start_child_loop(CPid, M, Msg)
.

start_child_loop(CPid, M, Msg) ->
  CPid ! {self(), Msg},
  receive
    {_, _} when M =:= 0 -> ok;
    {From, Message}     ->
      io:format("~n~s~p~s~s~s~w~s~p~n", ["\t\t", self(), " received ", Message, "#", M, " from", From]),
      start_child_loop(CPid, M-1, Msg)
  end
.