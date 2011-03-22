#!/usr/bin/env escript
%% -*- erlang -*-
%%! -name uxgen__test@127.0.0.1

-record(do_gen_in, {count = 200, to, urls}).
-record(do_freq_in, {data, fd}).
-record(do_regex_in, {re, to, mod}).

main([Ebin, InDir, OutDir]) -> 
	application:start(inets),
	{ok, FreqOutFd} = file:open(OutDir ++ "freq_dict.hrl", [write]),
	Pid = spawn(fun() -> do_freq end), 
    Pid ! {#do_freq_in{ data=dict:new(), fd=FreqOutFd }},

	Mod = [unicode,global,{capture,[2],list},dotall,ungreedy],
	RE = "<a\s[^>]*href=(\"??)(http://[^\" >]*?)\\1[^>]*>(.*)<\/a>",
	Pid1 = spawn(fun() -> do_regex() end),
    Pid1 ! {#do_regex_in{ re=RE, mod=Mod, to=false }},   

	Pid2 = spawn(fun() -> do_gen()   end), 
    Pid2 ! {#do_gen_in{ to=[Pid, Pid1], urls=dict:new() }},
	Pid1 ! {to, [Pid2]},

    wait(Pid ),
    wait(Pid1),
    wait(Pid2).  

wait(Pid) ->
    case process_info(Pid, status) of
        undefined -> ok;
        D -> io:format("~w~n", [D]), timer:sleep(1000), wait(Pid)
    end.

do_gen() ->
    receive
        {Data} -> do_gen(Data) 
    end.

%do_gen(Par) when Par#do_gen_in.count==0 -> send_all(Par#do_gen_in.to, [save]), do_gen(Par);
do_gen(Par) ->
	receive
	{test, Url} -> 
		io:format("~s", [Url]),
		do_gen(Par);
	{get, Url} when Par#do_gen_in.count>0 -> 
		io:format("~s~n", [Url]),
		case dict:is_key(Url, Par#do_gen_in.urls) of 
			false -> http:request(get, {Url, []}, [], [{sync, false}]),
				do_gen(Par#do_gen_in{ urls=dict:store(Url, true, Par#do_gen_in.urls) 
							, count=Par#do_gen_in.count-1});
			true -> do_gen(Par)
		end;
	{http, {ReqestId, 
		{{HttpVer, 200, Msg}, Headers, Body} = Result}} ->
		Str = unicode:characters_to_list(Body),
		%io:format("~w", [Str]),
		send_all(Par#do_gen_in.to, [{Str}]),
		io:format("OK", []),
		do_gen(Par#do_gen_in{  });
	Mess -> send_all(Par#do_gen_in.to, [Mess]), do_gen(Par)
	end.

send_all([], _) -> ok;
send_all([Dest|List], Mess) -> send_mess(Dest, Mess), send_all(List, Mess).

send_mess(_, []) -> ok;
send_mess(Dest, [Mess|List]) -> Dest ! Mess, send_mess(Dest, List).

do_freq() ->
    receive
        {Data} -> do_freq(Data) 
    end.

do_freq(Par) ->
	receive
		dump   -> to_file(dict:to_list(Par#do_freq_in.data), Par#do_freq_in.fd),
			do_freq(Par);
		save   -> io:format("~w", [dict:to_list(Par#do_freq_in.data)]),
			do_freq(Par);
		{Str}  when is_list(Str) ->
			do_freq(Par#do_freq_in{
				data = dict:merge(
					fun(Key, Val1, Val2) -> Val1+Val2 end
				,	Par#do_freq_in.data
				,	ux.string:freq(Str) 	
				)
			});
		_ -> do_freq(Par)
	end.

do_regex() ->
    receive
        {Data} -> do_regex(Data) 
    end.

do_regex(Par) ->
	receive
		{to, Pid} ->	
			do_regex(Par#do_regex_in{ to=Pid });
		{Str} when is_list(Str) -> 
			case re:urun(Str, 
				%"<a\s[^>]*href=(\"??)([^\" >]*?)\\1[^>]*>(.*)<\/a>",
				Par#do_regex_in.re, 
				Par#do_regex_in.mod) of
				nomatch -> ok;
				{match, Arr} -> send_all(Par#do_regex_in.to
			 			,	lists:map(fun([X]) -> {get, X} end, Arr))
						%io:format("~w", [lists:map(fun([X]) -> {test, X} end, Arr)])

			end,
			do_regex(Par);
		_ ->	do_regex(Par)
	end.

to_file(Dict, OutFd) -> to_file(Dict, OutFd, sum(Dict, 0)).
to_file([], OutFd, _) ->  io:format(OutFd, "freq_dict(_) -> 0. ~n", []);
to_file([H|T], OutFd, Sum) -> save(OutFd, H, Sum), to_file(T, OutFd, Sum).
save(OutFd, {Char, Count}, Sum) -> io:format(OutFd, "freq_dict(~w) -> ~w; ~n", [Char, Count/Sum]).

sum([], Sum) -> Sum;
sum([{Char, Count}|T], Sum) -> sum(T, Count+Sum).
