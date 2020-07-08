-module(cart).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, add/2, remove/2, show/1, checkout/1]).
-export([cancel/1, address/2, credit_card/3, delivered/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, cart_server).
-define(MENU_FILE, "menu.txt").

-record(server_state, {menu=[]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start_link(UserName) ->
  gen_server:call(?SERVER, {start_link, UserName}).

init([]) ->
  process_flag(trap_exit, true),
  {ok, [Menu|_]} = file:consult(?MENU_FILE),
  db:create_table(),
  {ok, #server_state{menu = Menu}}.

add(ReferenceId, Item) ->
  gen_server:call(?SERVER, {add, ReferenceId, Item}).

remove(ReferenceId, Item) ->
  gen_server:call(?SERVER, {remove, ReferenceId, Item}).

show(ReferenceId) ->
  gen_server:call(?SERVER, {show, ReferenceId}).

% TODO CHECKOUT FOR PAYMENT STATE
checkout(ReferenceId) ->
  gen_server:cast(?SERVER, {checkout, ReferenceId}).

cancel(ReferenceId) ->
  gen_server:cast(?SERVER, {cancel, ReferenceId}).

address(ReferenceId, Address) ->
  gen_server:call(?SERVER, {address, ReferenceId, Address}).

credit_card(ReferenceId, CCNumber, {ExpMo, ExpYr}) ->
  gen_server:call(?SERVER, {credit_card, ReferenceId, CCNumber, {ExpMo, ExpYr}}).

delivered(ReferenceId) ->
  gen_server:call(?SERVER, {delivered, ReferenceId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

handle_call({start_link, UserName}, _From, State) ->
  ReferenceId = make_ref(),
  spawn_link(cart_fsm, start_link, [UserName]),
  db:add_user(ReferenceId, UserName),
  {reply, {ok, ReferenceId}, State};

handle_call({add, ReferenceId, Item}, _From, State) ->
  case util:is_shop_item_valid(Item, State#server_state.menu) of
    false ->
      {reply, {error, unknown}, State};
    Price ->
      UserName = db:get_username(ReferenceId),
      cart_fsm:add(UserName, Item, Price),
      {reply, ok, State}
  end;

handle_call({remove, ReferenceId, Item}, _From, State) ->
  case util:is_shop_item_valid(Item, State#server_state.menu) of
    false ->
      {reply, {error, unknown}, State};
    Price ->
      UserName = db:get_username(ReferenceId),
      cart_fsm:remove(UserName, Item, Price),
      {reply, ok, State}
  end;

handle_call({show, ReferenceId}, _From, State) ->
  UserName = db:get_username(ReferenceId),
  {reply, cart_fsm:show(UserName), State};

handle_call({address, ReferenceId, Address}, _From, State) ->
  case util:is_address_valid(Address) of
    false ->
      {reply, {error, Address}, State};
    true ->
      UserName = db:get_username(ReferenceId),
      cart_fsm:address(UserName, Address),
      {reply, ok, State}
  end;

handle_call({credit_card, ReferenceId, CCNumber, {ExpMo, ExpYr}}, _From, State) ->
  case util:is_credit_card_valid(CCNumber, ExpMo, ExpYr) of
    false ->
      {reply, {error, card_invalid}, State};
    true ->
      UserName = db:get_username(ReferenceId),
      cart_fsm:credit_card(UserName, CCNumber, {ExpMo, ExpYr}),
      {reply, ok, State}
  end;

handle_call({delivered, ReferenceId}, _From, State) ->
  UserName = db:get_username(ReferenceId),
  {reply, cart_fsm:delivered(UserName), State}.

handle_cast({cancel, ReferenceId}, State) ->
  UserName = db:get_username(ReferenceId),
  cart_fsm:cancel(UserName),
  {noreply, State};

handle_cast({checkout, ReferenceId}, State) ->
  UserName = db:get_username(ReferenceId),
  cart_fsm:checkout(UserName),
  {noreply, State}.

% TODO DELETE USER FROM DB
handle_info({'EXIT', Pid, Reason}, State = #server_state{}) ->
  io:format("~s~w~s~n~p~n",
    ["Process ", Pid, " terminated with:", Reason]),
  %db:delete_user(),
  {noreply, State}.

% TODO KILL PROCESSES
terminate(_, _) ->
  db:clean_up(),
  ok.
