#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name uxgen__test@127.0.0.1

main([Ebin, InDir, OutDir]) -> 
    code:add_path(Ebin),

    % Minitest
    [<<0:8, 0:16, 0:16, 0:16, 0:16>>] = parseEl("[.0000.0000.0000.0000]"),
    [<<0:8, 1:16, 2:16, 3:16, 4:16>>] = parseEl("[.0001.0002.0003.0004]"),
    [<<0:8, 1:16, 2:16, 3:16, 4:16>>, <<1:8, 5:16, 6:16, 7:16, 8:16>>]
     = parseEl("[.0001.0002.0003.0004][*0005.0006.0007.0008]"),

	{ok, InFd}  = file:open(InDir  ++ "allkeys.txt", [read]),
	{ok, OutFd} = file:open(OutDir ++ "ducet.hrl",   [write]),

	do_gen(InFd, {OutFd}),

    io:format(OutFd, "ducet(_) -> false. ~n", []),
    ok.

do_gen(InFd, {OutFd} = OutFds) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, OutFds);
		{ok, Data} -> 
            case uxstring:explode(["#"], uxstring:delete_types([cc], Data)) of
                []       -> skip;
                [[]|_]   -> skip;
                [Row|_] ->
                    case uxstring:explode($;, Row) of
                        [Char, Element] ->
                            DucetEl = parseEl(uxstring:delete_types([zs], Element)),
                            io:format(OutFd, "ducet(16#~s) -> ~w; ~n", 
                                [uxstring:delete_types([zs], Char), 
                                % Reverse
                                DucetEl
                                ]);
                        _ -> skip
                    end
            end,
            do_gen(InFd, OutFds);
		eof -> ok
	end.
	
%% Parses "[.0000.0000.0000.0000]" to [<<0/8,0/32,0/32,0/32,0/32>>]
parseEl(El) -> lists:reverse(parseEl(El, [], false, [])).

% Buf - binary bufer
% Acc - string accumulator (f.e. [48,48,48,48])
parseEl([], _, _, Res) -> Res;
parseEl([$[, $. | Tail], _, _, Res) ->
    parseEl(Tail, [], <<0:8>>, Res); % [.XXXX.XXXX.XXXX.XXXX]
parseEl([$[, $* | Tail], _, _, Res) ->
    parseEl(Tail, [], <<1:8>>, Res); % [*XXXX.XXXX.XXXX.XXXX]
parseEl([_|Tail], Acc, false, Res) ->
    parseEl(Tail, Acc, false, Res);
parseEl([$]|Tail], Acc, Buf, Res) ->
    parseEl(Tail, [], false, [elRes(Acc, Buf)|Res]);
parseEl([$.|Tail], Acc, Buf, Res) ->
    parseEl(Tail, [], elRes(Acc, Buf), Res);
parseEl([H|Tail], Acc, Buf, Res) ->
    parseEl(Tail, [H|Acc], Buf, Res).

elRes(Acc, Buf) when length(Acc) == 5 ->
    Hex = uxstring:hex_to_int(lists:reverse(Acc)),
    <<Buf/binary,Hex:24>>;
elRes(Acc, Buf) when length(Acc) == 4 ->
    Hex = uxstring:hex_to_int(lists:reverse(Acc)),
    <<Buf/binary,Hex:16>>.


        
%% Blanked: Variable collation elements and any subsequent ignorables are reset 
%% so that their weights at levels one through three are zero. 
%% For example,
%%  SPACE would have the value [.0000.0000.0000]
%%  A combining grave accent after a space would have the value [.0000.0000.0000]
%%  Capital A would be unchanged, with the value [.06D9.0020.0008]
%%  A combining grave accent after a Capital A would be unchanged
%blanked(Char, El) ->
%    <<Flag:8, L1:16, L2:16, L3:16 | L4>> = El,
%    Type = uxstring:char_type(Char),
%    Ccc  = uxstring:ccc(Char),
%    case {Type, Ccc} of
%        {zs, _} -> <<Flag:8, 0:48, L4/binary>>
%    ehd.
