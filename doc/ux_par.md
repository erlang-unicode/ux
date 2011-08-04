Module ux_par
=============


<h1>Module ux_par</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


This Module contains functions for working with Req:parse_post() list        
from the mochiweb library.



<h2><a name="description">Description</a></h2>



Example:
[Extraction of params from a POST data list](https://github.com/freeakk/web_col).
<pre>   col_params(PostList) ->
V = ux_col:get_options([
{natural_sort, ux_par:atom("natural_sort", PostList)},
{case_sensitive, ux_par:atom("case_sensitive", PostList)},
{strength, ux_par:integer("strength", PostList)},
{alternate, ux_par:atom("alternate", PostList)},
{case_first, ux_par:atom("case_first", PostList)}
]).'</pre>

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#atom-2">atom/2</a></td><td></td></tr><tr><td valign="top"><a href="#el-2">el/2</a></td><td></td></tr><tr><td valign="top"><a href="#integer-2">integer/2</a></td><td></td></tr><tr><td valign="top"><a href="#string-2">string/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="atom-2"></a>

<h3>atom/2</h3>





`atom(Name, List) -> any()`

<a name="el-2"></a>

<h3>el/2</h3>





`el(Name, List) -> any()`

<a name="integer-2"></a>

<h3>integer/2</h3>





`integer(Name, List) -> any()`

<a name="string-2"></a>

<h3>string/2</h3>





`string(Name, List) -> any()`

