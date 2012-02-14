

#Module ux_opt_ranges#
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Functions for working with ranges in lists.

<a name="description"></a>

##Description##




ETS is fast only as a key-value store.       
But some data files contains ranges: From..To.       
The fastest way is using lists for storing this values.



There is two types of these lists:
* with booleans: `[{1,3}, 6, {8,9}]`. For example, `is_compat`;
* with values: `[{{1,3}, value1}, {{4,12}, value2}]`.

`in_list` function is for the first type.
`search` function is for the second type.
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#in_list-1">in_list/1</a></td><td></td></tr><tr><td valign="top"><a href="#search-2">search/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="in_list-1"></a>

###in_list/1##




`in_list(V) -> any()`

<a name="search-2"></a>

###search/2##




`search(Def, V) -> any()`

