

#Module ux_uca_utils#
* [Function Index](#index)
* [Function Details](#functions)




<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#do_alt-2">do_alt/2</a></td><td></td></tr><tr><td valign="top"><a href="#do_alt-3">do_alt/3</a></td><td></td></tr><tr><td valign="top"><a href="#do_extract-3">do_extract/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_ducet-0">get_ducet/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_options-0">get_options/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_reassign_function-2">get_reassign_function/2</a></td><td></td></tr><tr><td valign="top"><a href="#hangul_type-1">hangul_type/1</a></td><td></td></tr><tr><td valign="top"><a href="#implicit_type-1">implicit_type/1</a></td><td></td></tr><tr><td valign="top"><a href="#split_levels-3">split_levels/3</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="do_alt-2"></a>

###do_alt/2##




<pre>do_alt(A::function(), W::binary() | integer()) -&gt; [integer()]</pre>
<br></br>


<a name="do_alt-3"></a>

###do_alt/3##




`do_alt(A, W, S) -> any()`

<a name="do_extract-3"></a>

###do_extract/3##




<pre>do_extract(Uca_options::#uca_options{}, S::string(), D::function()) -&gt; {integer(), string()}</pre>
<br></br>


<a name="get_ducet-0"></a>

###get_ducet/0##




<pre>get_ducet() -&gt; function()</pre>
<br></br>


<a name="get_options-0"></a>

###get_options/0##




<pre>get_options() -&gt; #uca_options{}</pre>
<br></br>


<a name="get_reassign_function-2"></a>

###get_reassign_function/2##




`get_reassign_function(D, L) -> any()`

<a name="hangul_type-1"></a>

###hangul_type/1##




`hangul_type(X) -> any()`

<a name="implicit_type-1"></a>

###implicit_type/1##




`implicit_type(X) -> any()`

<a name="split_levels-3"></a>

###split_levels/3##




<pre>split_levels(L::integer(), B::boolean(), W::[[integer()]]) -&gt; {[integer()], [[integer()]]}</pre>
<br></br>


