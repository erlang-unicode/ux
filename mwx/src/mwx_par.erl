% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
%%% @doc This Module contains functions for working with Req:parse_post() list
%%%      from the mochiweb library.
%%%
%%% Example:
%%% [Extraction of params from a POST data list](https://github.com/freeakk/web_col).
%%% ```
%%% col_params(PostList) ->
%%%     V = ux_col:get_options([
%%%         {natural_sort, mwx_par:atom("natural_sort", PostList)},
%%%         {case_sensitive, mwx_par:atom("case_sensitive", PostList)},
%%%         {strength, mwx_par:integer("strength", PostList)},
%%%         {alternate, mwx_par:atom("alternate", PostList)},
%%%         {case_first, mwx_par:atom("case_first", PostList)}
%%%     ]).'
%%% '''
%%% @end
-module(mwx_par).
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

