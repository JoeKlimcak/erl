-module(ring).

%% API
-export([start/3, start_child/5]).

start(N, M, Msg) ->
  io:format("~s~p~n~n", ["SPAWNED PARENT @ ", self()]),
  CPid = spawn(ring, start_child, [self(), self(), N, M, Msg]),
  CPid ! {self(), Msg},

  receive
    {From, Msg} ->
      io:format("~n~s~w~s~s~s~w~s~p~n", ["PARENT @ ", self(), " received ", Msg, " ", M, "x Time from ", From]),
      ok
  end
.

start_child(Op, Parent, 1, M, Msg) ->
  io:format("~n~s~w~s~p~s~p~n", ["\t\tSPAWNED CHILD #", 1, " @", self(), " FROM ", Parent]),
  Op ! {self(), Msg},
  receive
    {From, Msg} ->
      io:format("~s~w~s~s~s~w~s~p~n", ["\t\t\t", self(), " received ", Msg, " ", M, "x Time from ", From]),
      ok
  end
;
start_child(OP, Parent, N, M, Msg) ->
  io:format("~s~w~s~p~s~p~n", ["\tSPAWNED CHILD #", N, " @", self(), " FROM ", Parent]),
  CPid = spawn(?MODULE, start_child, [OP, self(), N-1, M, Msg]),
  CPid ! {self(), Msg},
  receive
      {From, Msg} ->
        io:format("~s~w~s~s~s~w~s~p~n", ["\t\t", self(), " received ", Msg, " ", M, "x Time from ", From]),
        ok
  end
.