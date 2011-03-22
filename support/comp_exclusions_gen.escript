#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name uxgen__test@127.0.0.1

main([Ebin, InDir, OutDir]) -> 
    code:add_path(Ebin),

	{ok, InFd}  = file:open(InDir  ++ "CompositionExclusions.txt", [read]),
	{ok, OutFd} = file:open(OutDir ++ "is_comp_excl.hrl",          [write]),

	do_gen(InFd, {OutFd}),

    io:format(OutFd, "is_comp_excl(_) -> false. ~n", []),
    ok.

do_gen(InFd, {OutFd} = OutFds) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, OutFds);
		{ok, Data} -> 
            case uxstring:explode(["#"], ux.string:delete_types([cc], Data)) of
                []       -> skip;
                [[]|_]   -> skip;
                [Char|_] ->
                    io:format(OutFd, "is_comp_excl(16#~s) -> true; ~n", 
                        [uxstring:delete_types([zs], Char)])
            end,
            do_gen(InFd, OutFds);
		eof -> ok
	end.
	
