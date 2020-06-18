-module(hello_test).

-include_lib("eunit/include/eunit.hrl").
-import('hello', [func/1]).

simple_test() ->
  ?assert(true).

%another_test() ->
%  ?assertEqual(hello:func("hi2"), "hello hi2").
