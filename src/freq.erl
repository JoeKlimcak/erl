-module(freq).
-export([start/0, stop/0, allocate/0, deallocate/1, print/0]).
-export([init/0]).

%% These are the start functions used to create and %% initialize the server.
start() -> register(frequency, spawn(freq, init, [])).
init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies)
.
get_frequencies() -> [1,2,3].


stop() -> call(stop).
allocate() -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).
print() -> call(print).

call(Message) ->
  frequency ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end
.

%% The Main Loop
loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq, Pid),
      case element(1, NewFrequencies) of
        error ->
          reply(Pid, {error, element(2, NewFrequencies)}),
          loop(Frequencies);
        _     ->
          reply(Pid, ok),
          loop(NewFrequencies)
      end;
    {request, Pid, print} ->
      reply(Pid, Frequencies),
      loop(Frequencies);

    {request, Pid, stop} ->
      if
        length(element(2, Frequencies)) =:= 0 -> reply(Pid, ok);
        true ->
          reply(Pid, {error, not_empty}),
          loop(Frequencies)
      end
  end
.

reply(Pid, Reply) -> Pid ! {reply, Reply}.

allocate({[], Allocated}, _Pid) ->
  {
    {[], Allocated}, {error, no_frequency}
  }
;
allocate({[Freq|Free], Allocated}, Pid) ->
  {
    {Free, [{Freq, Pid}|Allocated]},
    {ok, Freq}
  }
.

deallocate({Free, Allocated}, Freq, Pid) ->
  IsMatch=lists:keyfind(Freq, 1, Allocated),
  if
    is_boolean(IsMatch) -> {error, no_match_found};
    is_tuple(IsMatch)   ->
       if
         element(2, IsMatch) =/= Pid -> {error, no_auth};
         true ->
           NewAllocated=lists:keydelete(Freq, 1, Allocated),
           {[Freq|Free], NewAllocated}
       end
  end
.