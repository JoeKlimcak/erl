-module(hello).

%% function test
-export([sum/1, sum/2, create/1, reverse_create/1, factorial/1, even/1, number/1]).
-export([print/1, print_even/1]).

sum(N) when N<0 -> 0;
sum(N) when N>0 -> N + sum(N-1);
sum(N)          -> N.

sum(N,M) ->
  case N =< M of
    true -> sum(M) - sum(N-1);
    false -> {error, abnormal}
  end.

create(N) -> create(N, []).
create(0, Acc) -> Acc;
create(N, Acc) -> create(N-1, [N|Acc]).

reverse_create(N) when N>1 -> [N|reverse_create(N-1)];
reverse_create(N)          -> [N].


factorial(N) when N > 0 -> N * factorial(N - 1);
factorial(0) -> 1.

even(Int) when Int rem 2 == 0 -> true;
even(Int) when Int rem 2 == 1 -> false.

number(Num) when is_integer(Num) -> integer;
number(Num) when is_float(Num) -> float;
number(_Other) -> false.

print(N) -> print_acc(1, N).
print_acc(Index, Boundary) when Index<Boundary ->
  io:format("Number:~p~n", [Index]),
  print_acc(Index+1, Boundary);
print_acc(Index, _) -> io:format("Number:~p~n", [Index]).

print_even(N) -> print_even_acc(2, N).
print_even_acc(Index, Boundary) when (Index<Boundary) and (Index rem 2 =:= 0) ->
  io:format("Number:~p~n", [Index]),
  print_even_acc(Index+1, Boundary);
print_even_acc(Index, Boundary) when (Index<Boundary) and (Index rem 2 =:= 1) ->
  print_even_acc(Index+1, Boundary);
print_even_acc(Index, _) when (Index rem 2 =:= 0) -> io:format("Number:~p~n", [Index]);
print_even_acc(_, _) -> '\n'.
