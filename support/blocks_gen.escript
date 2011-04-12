#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name uxgen__test@127.0.0.1

main([Ebin, InDir, OutDir]) -> 
    code:add_path(Ebin),

	{ok, InFd}  = file:open(InDir  ++ "Blocks.txt", [read]),
	{ok, OutFd} = file:open(OutDir ++ "blocks.hrl", [write]),

	do_gen(InFd, {OutFd}),

    io:format(OutFd, "char_block(_) -> other. ~n", []),
    ok.

do_gen(InFd, {OutFd} = OutFds) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, OutFds);
		{ok, Data} -> 
            case uxstring:explode(["#"], uxstring:delete_types([cc], Data)) of
                []       -> skip;
                [[]|_]   -> skip;
                [Chars|_] ->
                    case uxstring:explode(["..", ";"], Chars) of
                        [From, To, [_|Desk]] ->
                        io:format(OutFd, "char_block(Ch) when (Ch >= 16#~s) and (Ch =< 16#~s) -> ~s; ~n", 
                            lists:map(fun(Str) -> uxstring:delete_types([zs], Str) end, [From, To, 
                                [case X of
                                    $  -> $_;
                                    $- -> $_;
                                    _  -> X
                                 end || X <- uxstring:to_lower(Desk)]
                            ]));
                        _ -> ok
                    end
            end,
            do_gen(InFd, OutFds);
		eof -> ok
	end.
	
