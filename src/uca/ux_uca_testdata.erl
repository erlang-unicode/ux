%%% There are helpers for reading testdata.
%%% Used in ux_uca_tests and for generating BEAM module for ucol.
-module(ux_uca_testdata).
-export([read_line/2, read_shifted/0]).


read_shifted() ->
    Fd = ux_unidata:open_test_file('collation_test_shifted'),
    read_shifted_(skip(Fd), 0, []).


read_shifted_(Fd, Num, Acc) ->
    case read_line(Fd, Num) of
        {RawString, [], NewNum} ->
            read_shifted_(Fd, NewNum, Acc);
        {RawString, Points, NewNum} ->
            read_shifted_(Fd, NewNum, [Points|Acc]);
        eof -> lists:reverse(Acc) 
    end.


skip(Fd) ->
    case io:get_line(Fd, "") of
        "\n" -> Fd;
        X when is_list(X) -> skip(Fd)
    end.


%% @doc Read line from a testdata file Fd (see CollationTest.html).
%% Returns {Plain string, List of Codepoints, StrNum} or eof
%% Used by test/4.
%% @end
read_line(Fd, StrNum) ->
    case io:get_line(Fd, "") of
    eof -> eof;
    {error,Mess} -> throw({error, "Error while reading file", Mess});
    "#" ++ _Comment ->
        read_line(Fd, StrNum + 1);
    Data ->
        try % parse Data
            [Value|_] = ux_string:split(["#", ";", "\n"], Data),
            %% Converts "0009 0021" to [16#0009, 16#0021]
            Parsed = lists:map(fun ux_unidata_parser:hex_to_int/1,
                      string:tokens(Value, " ")),
            %% Delete false values.
            Res = [X || X <- Parsed, X =/= false],

            {Data, Res, StrNum + 1} % {FullStr, Codepaints}
        catch
        error:Reason ->
%           io:format(user, "~w: Data=~w ~n", [Reason, Data]),
            read_line(Fd, StrNum + 1)
        end
    end.

