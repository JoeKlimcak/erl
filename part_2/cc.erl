-module(cc).

%% API
-export([is_valid/3, transaction/4, cancel/1]).

is_valid(Address, CCNumber, Expiry) ->
  %false.
  true.

transaction(Address, CCNumber, Expiry, Price) ->
  % {error, invalid_card}
  %{error, funds}.
  {ok, 123456789}.

cancel(TrxId) ->
  % {error, unknown}
  ok.