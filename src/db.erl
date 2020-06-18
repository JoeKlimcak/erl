-module(db).

%% API
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

index_of(N, List) ->
  case N > length(List) of
    true -> not_found;
    false -> index_of_helper(N, List)
  end
.
index_of_helper(N, [Head|Tail]) ->
  if
    N =:= 1 -> Head;
    true    -> index_of_helper(N-1, Tail)
  end
.

find_key(_, List) when length(List) =:= 0 -> {error, not_found};
find_key(Key, List) ->
  [Head|Tail] = List,
  if
    Key =:= element(1, Head) -> {ok, element(2, Head)};
    true -> find_key(Key, Tail)
  end
.


new() -> [].

destroy(Db) -> ok.

write(Key, Element, Db) -> [{Key, Element} | Db].

read(Key, Db) -> find_key(Key, Db).

delete(Key, Db) ->
  Element = element(2, find_key(Key, Db)),
  New_Db = Db -- [{Key, Element}],
  New_Db
.

match(Element, Db) -> match_helper(Element, Db, []).
match_helper(_, Db, Acc) when length(Db) =:= 0 -> Acc;
match_helper(Element, Db, Acc) ->
  [Head|Tail] = Db,
  if
    Element =:= element(2, Head) ->
      New_Acc = [element(1, Head) | Acc],
      match_helper(Element, Tail, New_Acc);
    true -> match_helper(Element, Tail, Acc)
  end
.


