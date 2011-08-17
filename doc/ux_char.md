Module ux_char
==============


<h1>Module ux_char</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Character functions.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).

__See also:__ [ux](ux.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block-1">block/1</a></td><td></td></tr><tr><td valign="top"><a href="#comment-1">comment/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_acsii-1">is_acsii/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_cjk_compatibility_ideograph-1">is_cjk_compatibility_ideograph/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_cjk_unified_ideograph-1">is_cjk_unified_ideograph/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_decimal-1">is_decimal/1</a></td><td>Return true, if C is a decimal number.</td></tr><tr><td valign="top"><a href="#is_hangul-1">is_hangul/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_hangul_precomposed-1">is_hangul_precomposed/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_letter-1">is_letter/1</a></td><td>Returns true, if C is a letter.</td></tr><tr><td valign="top"><a href="#is_lower-1">is_lower/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_mark-1">is_mark/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_number-1">is_number/1</a></td><td>Returns true, if is C is a number.</td></tr><tr><td valign="top"><a href="#is_punctuation_mark-1">is_punctuation_mark/1</a></td><td>Returns true, if is C is a punctiation mark.</td></tr><tr><td valign="top"><a href="#is_separator-1">is_separator/1</a></td><td>Return true, if is C is a separator.</td></tr><tr><td valign="top"><a href="#is_unified_ideograph-1">is_unified_ideograph/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_upper-1">is_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_ncr-1">to_ncr/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="block-1"></a>

<h3>block/1</h3>





<pre>block(V::char) -> atom()</pre>
<br></br>


<a name="comment-1"></a>

<h3>comment/1</h3>





<pre>comment(V::char()) -> binary()</pre>
<br></br>


<a name="is_acsii-1"></a>

<h3>is_acsii/1</h3>





<pre>is_acsii(Char::char()) -> boolean()</pre>
<br></br>


<a name="is_cjk_compatibility_ideograph-1"></a>

<h3>is_cjk_compatibility_ideograph/1</h3>





`is_cjk_compatibility_ideograph(Ch) -> any()`

<a name="is_cjk_unified_ideograph-1"></a>

<h3>is_cjk_unified_ideograph/1</h3>





`is_cjk_unified_ideograph(Ch) -> any()`

<a name="is_decimal-1"></a>

<h3>is_decimal/1</h3>





<pre>is_decimal(C::char()) -> boolean()</pre>
<br></br>




Return true, if C is a decimal number.<a name="is_hangul-1"></a>

<h3>is_hangul/1</h3>





`is_hangul(Char) -> any()`

<a name="is_hangul_precomposed-1"></a>

<h3>is_hangul_precomposed/1</h3>





`is_hangul_precomposed(Char) -> any()`

<a name="is_letter-1"></a>

<h3>is_letter/1</h3>





<pre>is_letter(C::char()) -> boolean()</pre>
<br></br>




Returns true, if C is a letter.<a name="is_lower-1"></a>

<h3>is_lower/1</h3>





<pre>is_lower(V::char()) -> boolean()</pre>
<br></br>


<a name="is_mark-1"></a>

<h3>is_mark/1</h3>





`is_mark(C) -> any()`

<a name="is_number-1"></a>

<h3>is_number/1</h3>





<pre>is_number(C::char()) -> boolean()</pre>
<br></br>




Returns true, if is C is a number.<a name="is_punctuation_mark-1"></a>

<h3>is_punctuation_mark/1</h3>





<pre>is_punctuation_mark(C::char()) -> boolean()</pre>
<br></br>




Returns true, if is C is a punctiation mark.<a name="is_separator-1"></a>

<h3>is_separator/1</h3>





<pre>is_separator(C::char()) -> boolean()</pre>
<br></br>




Return true, if is C is a separator.<a name="is_unified_ideograph-1"></a>

<h3>is_unified_ideograph/1</h3>





`is_unified_ideograph(Ch) -> any()`

<a name="is_upper-1"></a>

<h3>is_upper/1</h3>





<pre>is_upper(V::char()) -> boolean()</pre>
<br></br>


<a name="to_lower-1"></a>

<h3>to_lower/1</h3>





<pre>to_lower(V::char()) -> char()</pre>
<br></br>


<a name="to_ncr-1"></a>

<h3>to_ncr/1</h3>





<pre>to_ncr(Char::char()) -> string()</pre>
<br></br>


<a name="to_upper-1"></a>

<h3>to_upper/1</h3>





<pre>to_upper(V::char()) -> char()</pre>
<br></br>


<a name="type-1"></a>

<h3>type/1</h3>





<pre>type(V::char()) -> <a href="#type-char_type">char_type()</a></pre>
<br></br>


