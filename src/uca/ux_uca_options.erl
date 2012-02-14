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
    C = get_options(),
    get_options(C, Params).

%% @doc If you want use this library without import *.hrl, you can create 
%% a #uca_options {} record with this function.
%% @end
get_options(C=#uca_options{}, 
    [{hangul_terminator, Val}|T]) ->
    NewC = C#uca_options{ hangul_terminator=Val },
    get_options(NewC, T);

get_options(C=#uca_options{}, 
    [{natural_sort, Val}|T]) ->
    NewC = C#uca_options{ natural_sort=Val },
    get_options(NewC, T);

get_options(C=#uca_options{}, 
    [{backwards, Val}|T]) ->
    NewC = C#uca_options{ backwards=Val },
    get_options(NewC, T);

get_options(C=#uca_options{}, 
    [{case_sensitive, Val}|T]) ->
    NewC = C#uca_options{ case_sensitive=Val },
    get_options(NewC, T);

get_options(C=#uca_options{}, 
    [{case_first, Val}|T]) ->
    NewC = C#uca_options{ case_first=Val },
    get_options(NewC, T);

get_options(C=#uca_options{}, 
    [{strength, Val}|T]) ->
    NewC = C#uca_options{ strength=Val },
    get_options(NewC, T);

get_options(C=#uca_options{}, 
    [{alternate, Val}|T]) ->
    NewC = C#uca_options{ alternate=Val },
    get_options(NewC, T);

get_options(C=#uca_options{}, 
    [{sort_key_format, Val}|T]) ->
    NewC = C#uca_options{ sort_key_format=Val },
    get_options(NewC, T);

get_options(C=#uca_options{}, []) ->
    C.

