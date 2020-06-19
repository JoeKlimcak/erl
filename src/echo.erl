-module(echo).

%% API
-export([start/0, print/2, stop/1, process_one/0]).

start() -> spawn(echo, process_one, []).

print(Pid, Msg) -> Pid ! {self(), Msg}.

stop(Pid) -> Pid ! stop.


process_one() ->
  receive
    start ->
      io:format("Recieved Start Command."),
      process_one();
    {From, Msg} ->
      io:format("Recieved ~p~s from ~n", [From, Msg]),
      process_one();
    stop ->
      io:format("Recieved Stop Command."),
      true
  end
.
