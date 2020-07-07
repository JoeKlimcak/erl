-module(util).

%% API
-export([is_shop_item_valid/2, is_address_valid/1, is_credit_card_valid/3]).

is_shop_item_valid(_, []) -> false;
is_shop_item_valid(Item, [{_, _, Item, Price}|_]) -> Price;
is_shop_item_valid(Item, [_|Tail]) -> is_shop_item_valid(Item, Tail).

is_credit_card_valid(CCNumber, ExpMo, ExpYr) ->
  case is_integer(CCNumber) and is_integer(ExpMo) and is_integer(ExpYr) of
    true -> true;
    false -> false
  end.

is_address_valid(Address) -> is_address_valid_helper(Address, [address, name, city, country]).
is_address_valid_helper([], []) -> true;
is_address_valid_helper([], _) -> false;
is_address_valid_helper([{Item, Value}|Tail], Necessary_info) ->
  case Item of
    address ->
      case is_tuple(Value) of
        true ->
          case is_integer(element(1, Value)) and is_list(element(2, Value)) of
            true ->
              Remaining_Necessary_Info = lists:delete(address, Necessary_info),
              is_address_valid_helper(Tail, Remaining_Necessary_Info);
            false -> false
          end
      end;
    name ->
      case is_list(Value) of
        true ->
          Remaining_Necessary_Info = lists:delete(name, Necessary_info),
          is_address_valid_helper(Tail, Remaining_Necessary_Info);
        false -> false
      end;
    city ->
      case is_list(Value) of
        true ->
          Remaining_Necessary_Info = lists:delete(city, Necessary_info),
          is_address_valid_helper(Tail, Remaining_Necessary_Info);
        false -> false
      end;
    country ->
      case is_list(Value) of
        true ->
          Remaining_Necessary_Info = lists:delete(country, Necessary_info),
          is_address_valid_helper(Tail, Remaining_Necessary_Info);
        false -> false
      end;
    _ -> false
  end.
