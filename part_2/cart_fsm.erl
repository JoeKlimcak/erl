-module(cart_fsm).

-behaviour(gen_statem).

%% API
-export([start_link/1, add/3, remove/3, show/1, checkout/1, cancel/1]).
-export([stop/1, address/2, credit_card/3]).

%% gen_statem callbacks
-export([init/1, format_status/2, terminate/3, callback_mode/0]).
-export([shopping/3, payment/3]).

-record(cart_state, {subtotal = 0,
                     basket=[],
                     address=[],
                     credit_card={}}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(UserName) ->
  gen_statem:start_link({local, UserName}, ?MODULE, [], []).

add(UserName, Item, Price) ->
  gen_statem:cast(UserName, {add, Item, Price}).

remove(UserName, Item, Price) ->
  gen_statem:cast(UserName, {remove, Item, Price}).

show(UserName) ->
  gen_statem:call(UserName, show).

checkout(UserName) ->
  gen_statem:cast(UserName, checkout).

cancel(UserName) ->
  gen_statem:cast(UserName, cancel).

address(UserName, Address) ->
  gen_statem:cast(UserName, {address, Address}).

credit_card(UserName, CCNumber, {ExpMo, ExpYr}) ->
  gen_statem:cast(UserName, {credit_card, CCNumber, {ExpMo, ExpYr}}).

%delivered(UserName) ->
%  gen_statem:call(UserName, delivered).

stop(UserName) ->
  gen_statem:stop(UserName).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
  {ok, shopping, #cart_state{}}.

callback_mode() ->
  state_functions.

%% @private
%% @doc Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
format_status(_Opt, [_PDict, _StateName, _State]) ->
  Status = some_term,
  Status.

shopping(cast, {add, Item, Price}, State) ->
  {
    keep_state,
    State#cart_state{
      basket = [Item|State#cart_state.basket],
      subtotal = State#cart_state.subtotal + Price
    }
  };

shopping(cast, {remove, Item, Price}, State) ->
  case lists:member(Item, State#cart_state.basket) of % CHECK IF ITEM IN BASKET
    false -> {keep_state, State};
    true ->
      {
        keep_state,
        State#cart_state{
          basket = lists:delete(Item, State#cart_state.basket),
          subtotal = State#cart_state.subtotal - Price
        }
      }
  end;

shopping({call, From}, show, State) ->
  {keep_state, State, [{reply, From, State}]};

shopping(cast, cancel, _) ->
  exit(normal);

shopping(cast, checkout, State) ->
  {next_state, payment, State};

shopping(EventType, _, State) ->
  io:format("WRONG EVENTYPE->"),
  io:format("~n"),
  io:format(EventType),
  %% Ignore all other events
  {keep_state,State}.

payment(cast, {address, Address}, State) ->
  {
    keep_state,
    State#cart_state{
      address = Address
    }
  };

payment(cast, {credit_card, CCNumber, {ExpMo, ExpYr}}, State) ->
  {
    keep_state,
    State#cart_state{
      credit_card = {CCNumber, {ExpMo, ExpYr}}
    }
  };

payment({call, From}, show, State) ->
  {keep_state, State, [{reply, From, State}]};

%%payment({call, From}, delivered, State) ->
%%  % TODO
%%  {next_state, shopping, State};

payment(cast, cancel, State) ->
  {next_state, shopping, State};

payment(cast, checkout, State) ->
  {next_state, shopping, State};

payment(EventType, _, State) ->
  io:format("WRONG EVENTYPE->"),
  io:format("~n"),
  io:format(EventType),
  %% Ignore all other events
  {keep_state,State}.

terminate(_, _, _) ->
  ok.

% TODO show() eventtype should be common to both states
