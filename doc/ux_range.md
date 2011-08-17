Module ux_range
===============


<h1>Module ux_range</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


ETS is fast only as a key-value store.



<h2><a name="description">Description</a></h2>



      
But some data files contains ranges: From..To.       
The fastest way is using lists for storing this values.



There is two types of these lists:
* with booleans: `[{1,3}, 6, {8,9}]`. For example, `is_compat`;
* with values: `[{{1,3}, value1}, {{4,12}, value2}]`.

`in_list` function is for the first type.
`search` function is for the second type.


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#in_list-2">in_list/2</a></td><td></td></tr><tr><td valign="top"><a href="#search-2">search/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="in_list-2"></a>

<h3>in_list/2</h3>





<pre>in_list(T::[{integer(), integer()} | integer()], V::integer()) -> boolean()</pre>
<br></br>


<a name="search-2"></a>

<h3>search/2</h3>





<pre>search(T::[{{integer(), integer()} | integer(), term()}], V::integer()) -> boolean()</pre>
<br></br>


