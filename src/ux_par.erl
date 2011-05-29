%%% This Module contains functions for a working with Req:parse_post() list.
-module(ux_par).
-export([el/2, string/2, atom/2, integer/2]).

el(Name, List) when is_list(Name) ->
    {_Key, Val} = lists:keyfind(Name, 1, List),
    Val.

string(Name, List) when is_list(Name) ->
    unicode:characters_to_list(list_to_binary(el(Name, List))).

atom(Name, List) when is_list(Name) ->
    list_to_existing_atom(el(Name, List)).

integer(Name, List) when is_list(Name) ->
    list_to_integer(el(Name, List)).

