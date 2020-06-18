-module(man).

%% API
-export([filter/2, reverse/1, concat/1, flatten/1]).

filter(List, N) -> filter_acc(List, N, []).
filter_acc(List, _, Acc) when length(List) =:= 0 -> Acc;
filter_acc([Head|Tail], N, Acc) ->
  if
    Head =< N -> New_Acc = [Head|Acc], filter_acc(Tail, N, New_Acc);
    true      -> filter_acc(Tail, N, Acc)
  end
.

reverse(List) -> reverse_acc(List, []).
reverse_acc(List, Acc) when length(List) =:= 1 ->
  [Head|_] = List,
  New_Acc = [Head|Acc],
  New_Acc
;
reverse_acc([Head|Tail], Acc) ->
  New_Acc = [Head|Acc],
  reverse_acc(Tail, New_Acc)
.

concat(List) -> reverse(concat_outer(List, [])).
concat_outer(List, Acc) when length(List) =:= 0 -> Acc;
concat_outer([Head|Tail] , Acc) ->
  New_Acc = concat_inner(Head, Acc),
  concat_outer(Tail, New_Acc)
.
concat_inner(List, Acc) when length(List) =:= 0 -> Acc;
concat_inner([Head|Tail], Acc) ->
  New_Acc = [Head|Acc],
  concat_inner(Tail, New_Acc)
.

flatten(List) -> reverse(flatten_acc(List, [])).
flatten_acc(List, Acc) when length(List) =:= 0 -> Acc;
flatten_acc(List, Acc) ->
  [Head|Tail] = List,
  try length(Head) of
    _ -> flatten_acc(Tail, flatten_acc(Head, Acc))
  catch
    _:_ -> flatten_acc(Tail, [Head|Acc])
  end
.
