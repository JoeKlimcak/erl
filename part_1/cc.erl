-module(cc).

-export([is_valid/3, transaction/4, cancel/1]).

is_valid(Address, CCNumber, Expiry) ->
  case rand:uniform() > 0.5 of
    true -> true;
    false -> false
  end.

transaction(Address, CCNumber, Expiry, Price) ->
  case rand:uniform() > 0.5 of
    true -> {ok, 123456789};
    false -> {error, invalid_card} %| {error, funds}
  end.

cancel(TrxId) ->
  case rand:uniform() > 0.5 of
    true ->
      % Credit the user
      ok;
    false -> {error, unknown}
  end.
