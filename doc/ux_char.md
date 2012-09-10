

#Module ux_char#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Character functions.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-char_type">char_type()</a>##



<pre>char_type() = <a href="ux_types.md#type-char_type">ux_types:char_type()</a></pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#block-1">block/1</a></td><td></td></tr><tr><td valign="top"><a href="#comment-1">comment/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_acsii-1">is_acsii/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_cjk_compatibility_ideograph-1">is_cjk_compatibility_ideograph/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_cjk_unified_ideograph-1">is_cjk_unified_ideograph/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_decimal-1">is_decimal/1</a></td><td>Return true, if C is a decimal number.</td></tr><tr><td valign="top"><a href="#is_hangul-1">is_hangul/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_hangul_precomposed-1">is_hangul_precomposed/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_letter-1">is_letter/1</a></td><td>Returns true, if C is a letter.</td></tr><tr><td valign="top"><a href="#is_lower-1">is_lower/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_mark-1">is_mark/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_number-1">is_number/1</a></td><td>Returns true, if is C is a number.</td></tr><tr><td valign="top"><a href="#is_punctuation_mark-1">is_punctuation_mark/1</a></td><td>Returns true, if is C is a punctiation mark.</td></tr><tr><td valign="top"><a href="#is_separator-1">is_separator/1</a></td><td>Return true, if is C is a separator.</td></tr><tr><td valign="top"><a href="#is_unified_ideograph-1">is_unified_ideograph/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_upper-1">is_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#script-1">script/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_ncr-1">to_ncr/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td></td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="block-1"></a>

###block/1##




<pre>block(V::char) -&gt; atom()</pre>
<br></br>


<a name="comment-1"></a>

###comment/1##




<pre>comment(V::char()) -&gt; binary()</pre>
<br></br>


<a name="is_acsii-1"></a>

###is_acsii/1##




<pre>is_acsii(Char::char()) -&gt; boolean()</pre>
<br></br>


<a name="is_cjk_compatibility_ideograph-1"></a>

###is_cjk_compatibility_ideograph/1##




`is_cjk_compatibility_ideograph(Ch) -> any()`

<a name="is_cjk_unified_ideograph-1"></a>

###is_cjk_unified_ideograph/1##




`is_cjk_unified_ideograph(Ch) -> any()`

<a name="is_decimal-1"></a>

###is_decimal/1##




<pre>is_decimal(C::char()) -&gt; boolean()</pre>
<br></br>




Return true, if C is a decimal number.<a name="is_hangul-1"></a>

###is_hangul/1##




`is_hangul(Char) -> any()`

<a name="is_hangul_precomposed-1"></a>

###is_hangul_precomposed/1##




`is_hangul_precomposed(Char) -> any()`

<a name="is_letter-1"></a>

###is_letter/1##




<pre>is_letter(C::char()) -&gt; boolean()</pre>
<br></br>




Returns true, if C is a letter.<a name="is_lower-1"></a>

###is_lower/1##




<pre>is_lower(V::char()) -&gt; boolean()</pre>
<br></br>


<a name="is_mark-1"></a>

###is_mark/1##




`is_mark(C) -> any()`

<a name="is_number-1"></a>

###is_number/1##




<pre>is_number(C::char()) -&gt; boolean()</pre>
<br></br>




Returns true, if is C is a number.<a name="is_punctuation_mark-1"></a>

###is_punctuation_mark/1##




<pre>is_punctuation_mark(C::char()) -&gt; boolean()</pre>
<br></br>




Returns true, if is C is a punctiation mark.<a name="is_separator-1"></a>

###is_separator/1##




<pre>is_separator(C::char()) -&gt; boolean()</pre>
<br></br>




Return true, if is C is a separator.<a name="is_unified_ideograph-1"></a>

###is_unified_ideograph/1##




`is_unified_ideograph(Ch) -> any()`

<a name="is_upper-1"></a>

###is_upper/1##




<pre>is_upper(V::char()) -&gt; boolean()</pre>
<br></br>


<a name="script-1"></a>

###script/1##




<pre>script(V::char) -&gt; atom()</pre>
<br></br>


<a name="to_lower-1"></a>

###to_lower/1##




<pre>to_lower(V::char()) -&gt; char()</pre>
<br></br>


<a name="to_ncr-1"></a>

###to_ncr/1##




<pre>to_ncr(Char::char()) -&gt; string()</pre>
<br></br>


<a name="to_upper-1"></a>

###to_upper/1##




<pre>to_upper(V::char()) -&gt; char()</pre>
<br></br>


<a name="type-1"></a>

###type/1##




<pre>type(V::char()) -> <a href="#type-char_type">char_type()</a></pre>
<br></br>


