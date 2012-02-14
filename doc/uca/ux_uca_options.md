

#Module ux_uca_options#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


This library contains functions for manipulating with
a configuration of sorting.

<a name="description"></a>

##Description##
        You can use it as:
`C = ux_uca_options:get_options(shifted).`
And then:
`ux_uca:sort(C, ["string1", "string2", "string3").`<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_options-0">get_options/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_options-1">get_options/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_options-2">get_options/2</a></td><td>If you want use this library without import *.hrl, you can create
a #uca_options {} record with this function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="get_options-0"></a>

###get_options/0##




`get_options() -> any()`

<a name="get_options-1"></a>

###get_options/1##




`get_options(Params) -> any()`

<a name="get_options-2"></a>

###get_options/2##




`get_options(C, T) -> any()`



If you want use this library without import *.hrl, you can create
a #uca_options {} record with this function.