-module(cart_supervisor).

-export([start_child/2, stop_child/1]).

start_child(ReferenceId, {Mod, Fun, Argos})-> ok.

stop_child(ReferenceId)-> ok.