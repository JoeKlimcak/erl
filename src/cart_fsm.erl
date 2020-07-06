% FINITE STATE MACHINE
-module(cart_fsm).

-export([shopping/2, payment/3, delivery/1]).
-export([init/0, transaction/2, is_valid/1]).

-record(billing_info, {address = null, credit_card = null}).
-record(transaction, {id = null, total = null}).

init() ->
  shopping(0, []).

shopping(Total, Cart) ->
  receive
    {add, Item, Price} ->
      shopping(Total + Price, [Item|Cart]);

    {remove, Item, Price} ->
      case lists:member(Item, Cart) of
        true ->                    % REMOVE ITEM FROM SHOPPING CART
          shopping(Total - Price, lists:delete(Item, Cart));
        false ->                   % ITEM NOT PART OF CART -> CONTINUE SHOPPING CART
          shopping(Total, Cart)
      end;

    {checkout, From} ->            % CHECKOUT -> MOVE TO PAYMENT STATE
      From ! ok,
      payment(Total, Cart, #billing_info{});

    {show, From} ->
      From ! {ok, Cart, Total},
      shopping(Total, Cart);

    {cancel, From} ->
      From ! delete                % CANCELLING SHOPPING -> TERMINATE
  end.

payment(Total, Cart, BillingInfo) ->
  receive
    {address, Address} ->
      payment(Total, Cart, BillingInfo#billing_info{address = Address});

    {credit_card, CreditCard} ->
      payment(Total, Cart, BillingInfo#billing_info{credit_card = CreditCard});

    {checkout, From} ->
      case is_list(BillingInfo#billing_info.address) and is_tuple(BillingInfo#billing_info.credit_card) of

        true ->
          case is_valid(BillingInfo) of

            true ->
              case transaction(Total, BillingInfo) of
                {ok, TrxId} ->    % TRANSACTION SUCCESS -> MOVE TO DELIVERY STATE
                  From ! ok,
                  delivery(#transaction{id = TrxId, total = Total});
                {error, _} ->     % CREDIT_INFO ERROR -> TRANSACTION UNSUCCESSFUL, CONTINUE PAYMENT STATE
                  From ! {error, credit_info},
                  payment(Total, Cart, BillingInfo)
              end;

            false ->              % CREDIT_INFO ERROR -> INVALID CARD FROM CC MODULE, CONTINUE PAYMENT STATE
              From ! {error, credit_info},
              payment(Total, Cart, BillingInfo)

          end;

        false ->                  % BILLING_INFO ERROR -> MISSING ADDRESS OR CREDIT_CARD, CONTINUE PAYMENT STATE
          From ! {error, billing_info},
          payment(Total, Cart, BillingInfo)

      end;

    {show, From} ->
      From ! {ok, Cart, Total},
      payment(Total, Cart, BillingInfo);

    {cancel, From} ->             % CANCEL PAYMENT -> RETURN TO SHOPPING STATE
      From ! ok,
      shopping(Total, Cart)
  end.

delivery(Transaction) ->
  receive
    {cancel, From} ->
      case cancel(Transaction) of
        ok ->                     % CANCEL SUCCESS -> CALL REFUND METHOD FROM CREDIT CARD API, AND TERMINATE
          From ! ok;
        {error, unknown} ->       % CANCEL ERROR -> CONTINUE DELIVERY STATE
          From ! {error, unknown},
          delivery(Transaction)
      end;
    {delivered, From} ->          % DELIVERY SUCCESS -> TERMINATE
      From ! ok

  after 10000 ->                % TIMEOUT -> TERMINATE
    erlang:error(timeout)

  end.


% HELPER FUNCTIONS TO ABSTRACT CALLS TO CC MODULE
is_valid(BillingInfo) ->
  cc:is_valid(BillingInfo#billing_info.address,
              element(1, BillingInfo#billing_info.credit_card),
              element(2, BillingInfo#billing_info.credit_card)).

transaction(Total, BillingInfo) ->
  cc:transaction(BillingInfo#billing_info.address,
                 element(1, BillingInfo#billing_info.credit_card),
                 element(2, BillingInfo#billing_info.credit_card),
                 Total).

cancel(Transaction) ->
  cc:cancel(Transaction#transaction.id).