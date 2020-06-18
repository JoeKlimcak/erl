-module(db_test).

-include_lib("eunit/include/eunit.hrl").
-import('db', []).

new_test() ->
  ?assertEqual(db:new(), []).

destroy_test() ->
  Db = db:new(),
  ?assertEqual(db:destroy(Db), ok).

write_test() ->
  Db = db:new(),
  ?assertEqual(db:write(francesco, london, Db), [{francesco,london}]),
  Db1 = db:write(francesco, london, Db),
  ?assertEqual(db:write(lelle, stockholm, Db1), [{lelle,stockholm},{francesco,london}]).

read_test() ->
  Db = db:new(),
  Db1 = db:write(francesco, london, Db),
  Db2 = db:write(lelle, stockholm, Db1),
  ?assertEqual(db:read(francesco, Db2), {ok,london}),
  ?assertEqual(db:read(joe, Db2), {error,not_found}).

delete_test() ->
  Db = db:new(),
  Db1 = db:write(francesco, london, Db),
  Db2 = db:write(lelle, stockholm, Db1),
  Db3 = db:write(joern, stockholm, Db2),
  ?assertEqual(db:delete(lelle, Db3), [{joern,stockholm},{francesco,london}]).

match_test() ->
  Db = db:new(),
  Db1 = db:write(francesco, london, Db),
  Db2 = db:write(lelle, stockholm, Db1),
  Db3 = db:write(joern, stockholm, Db2),
  ?assertEqual(db:match(stockholm, Db3), [lelle,joern]).