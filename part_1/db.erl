-module(db).

-export([create_table/0, clean_up/0, add_user/3, delete_user/1]).
-export([check_user/1, get_username/1, get_ref_id_from_pid/1]).

create_table() ->
  ets:new(db, [named_table, private]).

clean_up() ->
  ets:delete(db).

add_user(ReferenceId, Pid, UserName) ->
  ets:insert(db, {ReferenceId, Pid, UserName}).

delete_user(ReferenceId) ->
  ets:delete(db, ReferenceId).

check_user(ReferenceId) ->
  case ets:lookup(db, ReferenceId) of
    [] -> false;
    [{_, _, _}] -> true
  end.

get_username(ReferenceId) ->
  [{_, _, UserName}] = ets:lookup(db, ReferenceId),
  UserName.

get_ref_id_from_pid(Pid) ->
  [[ReferenceId]] = ets:match(db, {'$1', Pid, '_'}),
  ReferenceId.