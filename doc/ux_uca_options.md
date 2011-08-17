Module ux_uca_options
=====================


<h1>Module ux_uca_options</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


This library contains functions for manipulating with
a configuration of sorting.



<h2><a name="description">Description</a></h2>

        You can use it as:
`C = ux_uca_options:get_options(shifted).`
And then:
`ux_uca:sort(C, ["string1", "string2", "string3").`

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_options-0">get_options/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_options-1">get_options/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_options-2">get_options/2</a></td><td>If you want use this library without import *.hrl, you can create
a #uca_options {} record with this function.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="get_options-0"></a>

<h3>get_options/0</h3>





`get_options() -> any()`

<a name="get_options-1"></a>

<h3>get_options/1</h3>





`get_options(Params) -> any()`

<a name="get_options-2"></a>

<h3>get_options/2</h3>





`get_options(C, T) -> any()`



If you want use this library without import *.hrl, you can create
a #uca_options {} record with this function.