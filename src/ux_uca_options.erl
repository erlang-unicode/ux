%%% @doc This library contains functions for manipulating with
%%%      a configuration of sorting.
%%%      You can use it as:
%%%      `C = ux_uca_options:get_options(shifted).'
%%%      And then:
%%%      `ux_uca:sort(C, ["string1", "string2", "string3").'
%%% @end

-module(ux_uca_options).
-export([get_options/0, get_options/1, get_options/2]).
-include("ux_uca.hrl").

get_options() -> #uca_options{ }.

get_options(non_ignorable) ->
    #uca_options { 
        natural_sort = false,
        strength = 3,
        alternate = non_ignorable
    };
get_options(blanked) ->
    #uca_options { 
        natural_sort = false,
        strength = 3,
        alternate = blanked
    };
get_options(shifted) ->
    #uca_options { 
        natural_sort = false,
        strength = 4,
        alternate = shifted
    };
get_options(shift_trimmed) ->
    #uca_options { 
        natural_sort = false,
        strength = 4,
        alternate = shift_trimmed 
    };
get_options([_|_] = Params) ->
    get_options(Params, get_options()).

%% @doc If you want use this library without import *.hrl, you can create 
%% a #uca_options {} record with this function.
%% @end
get_options([{hangul_terminator, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ hangul_terminator=Val });
get_options([{natural_sort, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ natural_sort=Val });
get_options([{case_sensitive, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ case_sensitive=Val });
get_options([{case_first, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ case_first=Val });
get_options([{strength, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ strength=Val });
get_options([{alternate, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ alternate=Val });
get_options([{sort_key_format, Val}|T], Opt = #uca_options{ }) ->
    get_options(T, Opt#uca_options{ sort_key_format=Val });
get_options([], Opt = #uca_options{ }) ->
    Opt.

