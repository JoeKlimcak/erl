%%% File : db.erl
%%% Description : Database API for customer DB
-module(db).

-export([create_table/0, clean_up/0, add_user/3, delete_user/1]).
-export([get_user_pid/1, get_username/1]).

create_table() ->
  ets:new(db, [named_table, public]).

clean_up() ->
  ets:delete(db).

add_user(RefId, Pid, UserName) ->
  ets:insert(db, {RefId, Pid, UserName}).

delete_user(RefId) ->
  ets:delete(db, RefId).

get_user_pid(RefId) ->
  [{_, Pid, _}] = ets:lookup(db, RefId),
  Pid.

get_username(RefId) ->
  [{_, _, UserName}] = ets:lookup(db, RefId),
  UserName.