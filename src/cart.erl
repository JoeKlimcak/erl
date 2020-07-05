-module(cart).
-behaviour(gen_server).

-export([start/0, start_link/1, add/2, remove/2, checkout/1]).
-export([address/2, credit_card/3, delivered/1, cancel/1, show/1]).

-export([handle_call/3]).
-export([init/1]).

start() ->
  {ok, [Menu|_]} = file:consult("menu.txt"),
  gen_server:start({local, ?MODULE}, ?MODULE, [Menu], []).

start_link(UserName) ->
  gen_server:call(?MODULE, {start_link, UserName}).

add(ReferenceId, Item) ->
  gen_server:call(?MODULE, {add, ReferenceId, Item}).

remove(ReferenceId, Item) ->
  gen_server:call(?MODULE, {remove, ReferenceId, Item}).

checkout(ReferenceId) ->
  gen_server:call(?MODULE, {checkout, ReferenceId}).

address(ReferenceId, Address) ->
  gen_server:call(?MODULE, {address, ReferenceId, Address}).

credit_card(ReferenceId, CCNumber, {ExpMo, ExpYr}) ->
  gen_server:call(?MODULE, {credit_card, ReferenceId, CCNumber, {ExpMo, ExpYr}}).

delivered(ReferenceId) ->
  gen_server:call(?MODULE, {delivered, ReferenceId}).

cancel(ReferenceId) ->
  gen_server:call(?MODULE, {cancel, ReferenceId}).

show(ReferenceId) ->
  gen_server:call(?MODULE, {show, ReferenceId}).

init([Menu]) ->
  db:create_table(),
  {ok, [Menu]}.

handle_call({start_link, UserName}, _From, [Menu]) ->
  ReferenceId = make_ref(),
  Pid = spawn_link(cart_fsm, init, []),
  db:add_user(ReferenceId, Pid, UserName),
  {reply, {ok, ReferenceId}, [Menu]};

handle_call({add, ReferenceId, Item}, _From, [Menu]) ->
  Pid = db:get_user_pid(ReferenceId),
  case util:is_shop_item_valid(Item, Menu) of
    false ->
      {reply, {error, unknown}, [Menu]};
    Price ->
      Pid ! {add, Item, Price},
      {reply, ok, [Menu]}
  end;

handle_call({remove, ReferenceId, Item}, _From, [Menu]) ->
  Pid = db:get_user_pid(ReferenceId),
  case util:is_shop_item_valid(Item, Menu) of
    false ->
      {reply, {error, unknown}, [Menu]};
    Price ->
      Pid ! {remove, Item, Price},
      {reply, ok, [Menu]}
  end;

handle_call({address, ReferenceId, Address}, _From, [Menu]) ->
  Pid = db:get_user_pid(ReferenceId),
  case util:is_address_valid(Address) of
    false ->
      {reply, {error, Address}, [Menu]};
    true ->
      Pid ! {address, Address},
      {reply, ok, [Menu]}
  end;

handle_call({credit_card, ReferenceId, CCNumber, {ExpMo, ExpYr}}, _From, [Menu]) ->
  Pid = db:get_user_pid(ReferenceId),
  case util:is_credit_card_valid(CCNumber, ExpMo, ExpYr) of
    false ->
      {reply, {error, card_invalid}, [Menu]};
    true ->
      Pid ! {credit_card, {CCNumber, {ExpMo, ExpYr}}},
      {reply, ok, [Menu]}
  end;

handle_call({delivered, ReferenceId}, _From, [Menu]) ->
  Pid = db:get_user_pid(ReferenceId),
  Pid ! {delivered, self()},

  receive
    ok ->
      {reply, ok, [Menu]};
    _ ->
      {reply, {error, unknown}, [Menu]}
  end;

handle_call({checkout, ReferenceId}, _From, [Menu]) ->
  Pid = db:get_user_pid(ReferenceId),
  Pid ! {checkout, self()},

  receive
    ok ->
      {reply, ok, [Menu]};
    {error, Error} ->
      {reply, {error, Error}, [Menu]}
  end;

handle_call({cancel, ReferenceId}, _From, [Menu]) ->
  Pid = db:get_user_pid(ReferenceId),
  Pid ! {cancel, self()},

  receive
    ok ->
      {reply, ok, [Menu]};
    delete ->
      db:delete_user(ReferenceId),
      {reply, ok, [Menu]};
    {error, Error} ->
      {reply, {error, Error}, [Menu]};
    _ ->
      {reply, {error, unknown}, [Menu]}
  end;

handle_call({show, ReferenceId}, _From, [Menu]) ->
  Pid = db:get_user_pid(ReferenceId),
  UserName = db:get_username(ReferenceId),
  Pid ! {show, self()},

  receive
    {ok, Items, Total} ->
      io:format("~n~s~s~n~s~p~n~s~w~n~n",
        ["Username: ", UserName, "Shopping Cart: ", Items, "Subtotal: ", Total]),
      {reply, ok, [Menu]};
    {error, Error} ->
      {reply, {error, Error}, [Menu]}
  end.

