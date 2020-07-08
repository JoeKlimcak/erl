%%% File : db.erl
%%% Description : Database API for customer DB
-module(db).

-export([create_table/0, clean_up/0, add_user/2, delete_user/1]).
-export([get_username/1]).

create_table() ->
  ets:new(db, [named_table, public]).

clean_up() ->
  ets:delete(db).

add_user(ReferenceId, UserName) ->
  ets:insert(db, {ReferenceId, UserName}).

delete_user(ReferenceId) ->
  ets:delete(db, ReferenceId).

get_username(ReferenceId) ->
  [{_, UserName}] = ets:lookup(db, ReferenceId),
  UserName.

%%get_user_pid(ReferenceId) ->
%%  [{_, Pid, _}] = ets:lookup(db, ReferenceId),
%%  Pid.