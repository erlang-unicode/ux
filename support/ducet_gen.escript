#!/usr/bin/env escript
% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
%% -*- erlang -*-
%%! -name uxgen__test@127.0.0.1

main([Ebin, InDir, OutDir]) -> 
    code:add_path(Ebin),

    % Minitest
    [<<0:8, 0:16, 0:16, 0:16, 0:16>>] = parseEl("[.0000.0000.0000.0000]"),
    [<<0:8, 1:16, 2:16, 3:16, 4:16>>] = parseEl("[.0001.0002.0003.0004]"),
    [<<0:8, 1:16, 2:16, 3:16, 4:16>>, <<1:8, 5:16, 6:16, 7:16, 8:16>>]
     = parseEl("[.0001.0002.0003.0004][*0005.0006.0007.0008]"),

    {[1,2,3], 4} = last([1,2,3,4]),
    {[     ], 4} = last([4]),

    % Open fds
	{ok, InFd}  = file:open(InDir  ++ "allkeys.txt", [read]),
	{ok, OutFd} = file:open(OutDir ++ "ducet.hrl",   [write]),

	Chars = do_gen(InFd, {OutFd}, []),
    do_more({OutFd}, Chars, Chars),

    io:format(OutFd, "ducet_r(_) -> other. ~n", []),
    ok.

do_more({OutFd} = OutFds, [], Res) -> Res;
do_more({OutFd} = OutFds, [Char | Chars], AddedOld) ->
    AddedNew = do_more1(Char, OutFd, AddedOld),
    do_more(OutFds, Chars, AddedNew).

do_more1([], _, Res) -> Res;
do_more1([_|_] = CodePaints, Fd, Added) ->
    {Head, _} = last(CodePaints),
    case lists:member(Head, Added) of % Was added before?
           false -> io:format(Fd, "ducet_r(~w) -> more; ~n", [lists:reverse(Head)]),
                    do_more1(Head, Fd, [Head|Added]);
           true  -> do_more1(Head, Fd, Added)
    end.
                
% Example:
% [1,2,3,4] => {[1,2,3], 4}
% [4]       => {[], 4}
last(Els) ->
    last1(Els, []).
last1([Head|[_|_] = Tail], Els) ->
    last1(Tail, [Head|Els]);
last1([LastEl], Els) ->
    {lists:reverse(Els), LastEl}.

do_gen(InFd, {OutFd} = OutFds, Chars) ->
	case file:read_line(InFd) of
		{ok, []} ->
			do_gen(InFd, OutFds, Chars);
		{ok, Data} -> 
            case ux_string:explode(["#"], ux_string:delete_types([cc], Data)) of
                []       -> do_gen(InFd, OutFds, Chars);
                [[]|_]   -> do_gen(InFd, OutFds, Chars);
                [Row|_] ->
                    case ux_string:explode($;, Row) of
                        [Char, Element] ->
                            OutEl = parseEl(ux_string:delete_types([zs], Element)),
                            InEl  = %ux_string:to_nfd
                                (lists:map(fun ux_string:hex_to_int/1, string:tokens(Char, " "))),


                            case lists:member(InEl, Chars) of
                                false -> io:format(OutFd, "ducet_r(~w) -> ~w; ~n", 
                                             [lists:reverse(InEl), OutEl]),
                                         do_gen(InFd, OutFds, [InEl|Chars]);
                                true  -> do_gen(InFd, OutFds, Chars)
                            end;
                        _ -> do_gen(InFd, OutFds, Chars)
                    end
            end;
		eof -> Chars 
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
    Hex = ux_string:hex_to_int(lists:reverse(Acc)),
    <<Buf/binary,Hex:24>>;
elRes(Acc, Buf) when length(Acc) == 4 ->
    Hex = ux_string:hex_to_int(lists:reverse(Acc)),
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
%    Type = ux_string:char_type(Char),
%    Ccc  = ux_string:ccc(Char),
%    case {Type, Ccc} of
%        {zs, _} -> <<Flag:8, 0:48, L4/binary>>
%    ehd.
