-module(cart_fsm).

-behaviour(gen_statem).

%% API
-export([start_link/1, add/3, remove/3, show/1, checkout/1, cancel/1]).
-export([stop/1, address/2, credit_card/3, delivered/1]).

%% gen_statem callbacks
-export([init/1, terminate/3, callback_mode/0]).
-export([shopping/3, payment/3, delivery/3]).

-record(cart_state, {subtotal = 0,
                     basket=[],
                     address=[],
                     cc_number = null,
                     expiry={},
                     trx_id = null}).

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
  gen_statem:call(UserName, checkout).

cancel(UserName) ->
  gen_statem:cast(UserName, cancel).

address(UserName, Address) ->
  gen_statem:cast(UserName, {address, Address}).

credit_card(UserName, CCNumber, {ExpMo, ExpYr}) ->
  gen_statem:cast(UserName, {credit_card, CCNumber, {ExpMo, ExpYr}}).

delivered(UserName) ->
  gen_statem:cast(UserName, delivered).

stop(UserName) ->
  gen_statem:stop(UserName).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

init([]) ->
  {ok, shopping, #cart_state{}}.

callback_mode() ->
  state_functions.

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

shopping(cast, cancel, _) ->
  terminate_shopping_session();

shopping({call, From}, checkout, State) ->
  {next_state, payment, State, [{reply, From, ok}]};

shopping(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

payment(cast, {address, Address}, State) ->
  {
    keep_state,
    State#cart_state{
      address = Address
    }
  };

payment(cast, {credit_card, CCNumber, Expiry}, State) ->
  {
    keep_state,
    State#cart_state{
      cc_number = CCNumber,
      expiry = Expiry
    }
  };

payment({call, From}, show, State) ->
  {keep_state, State, [{reply, From, State}]};

payment(cast, cancel, State) ->
  {next_state, shopping, State};

payment({call, From}, checkout, State = #cart_state{address = []}) ->
  {keep_state, State, [{reply, From, {error, billing_info}}]};

payment({call, From}, checkout, State = #cart_state{cc_number = null}) ->
  {keep_state, State, [{reply, From, {error, billing_info}}]};

payment({call, From}, checkout, State) ->
  case is_valid(State) of
    false ->
      {keep_state, State, [{reply, From, {error, credit_info}}]};
    true ->
      case transaction(State) of
        {error, _} ->
          {keep_state, State, [{reply, From, {error, credit_info}}]};
        {ok, TrxId} ->
          {
            next_state,
            delivery,
            State#cart_state{
              trx_id = TrxId
            },
            [
              {timeout, 3600000, timeout_event},
              {reply, From, ok}
            ]
          }
      end
  end;

payment(EventType, EventContent, Data) ->
  handle_event(EventType, EventContent, Data).

delivery(cast, delivered, _) ->
  terminate_shopping_session();

delivery(timeout, _, State) ->
  refund(State),
  terminate_shopping_session().

terminate(_, _, _) ->
  ok.

%% Show Basket and SubTotal
handle_event({call,From}, show, State) ->
  {keep_state, State, [{reply, From, {
    State#cart_state.subtotal, State#cart_state.basket
  }}]}.

%%%===================================================================
%%% internal functions
%%%===================================================================

is_valid(State) ->
  cc:is_valid(State#cart_state.address,
              State#cart_state.cc_number,
              State#cart_state.expiry).

transaction(State) ->
  cc:transaction(State#cart_state.address,
                 State#cart_state.cc_number,
                 State#cart_state.expiry,
                 State#cart_state.subtotal).

refund(State) ->
  cc:cancel(State#cart_state.trx_id).

terminate_shopping_session() ->
  exit(normal).