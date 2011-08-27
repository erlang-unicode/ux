-module(ux_cldr_support).
-export([make_hrl_files/0]).

make_hrl_files() ->
    {ok, OldCWD} = file:get_cwd(),
    ok = file:set_cwd("/tmp"),
    Files = ["ldml"],
    ok = extract_files(Files),
    ok = file:set_cwd("/tmp/common/dtd"),
        
    XsdFn = fun dtd_to_xsd/1,
    HrlFn = xsd_to_hrl(lib_dir()),
    CopyFn = fun copy_xsd/1,
    
    % Convert dtd to xsd:
    lists:map(XsdFn, Files),

    % Copy *.xsd to priv/ directory:
    lists:map(CopyFn, Files),

    % Convert dtd to hrl:
    Res = lists:map(HrlFn, Files),

    file:set_cwd(OldCWD),
    {ok, Res}.


lib_dir() ->
    code:lib_dir(ux, src) ++ "/erlsom".


extract_files(List) ->
    Archive = ux_unidata:get_source_file('cldr_core'),
    NewList = lists:map(fun(X) ->
            "common/dtd/" ++ X ++ ".dtd" end, List),
    Options = [{file_list, NewList}],
    zip:extract(Archive, Options),
    ok.


dtd_to_xsd(File) ->
    From = File ++ ".dtd",
    To = File ++ ".xsd",
    dtd_to_xsd(From, To).

dtd_to_xsd(From, To) ->
    os:cmd("trang -I dtd -O xsd " ++ From ++ " " ++ To).

xsd_to_hrl(LibDir) ->
    fun(File) ->
        From = File ++ ".xsd",
        To = LibDir ++ "/" ++ File ++ ".hrl",
        xsd_to_hrl(From, To)
    end.

xsd_to_hrl(From, To) ->
    Options = [],
    ok = erlsom:write_xsd_hrl_file(From, To, []),
    To.

copy_xsd(File) ->
    Src = File ++ ".xsd",
    Dest = ux_unidata:get_xsd_file(list_to_existing_atom(File)),
    {ok, _Bytes} = file:copy(Src, Dest),
    Dest.

