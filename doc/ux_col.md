Module ux_col
=============


<h1>Module ux_col</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


UCA.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).

__See also:__ [ux](ux.md).

<h2><a name="description">Description</a></h2>





<h3><a name="Additional_information_(and_links)">Additional information (and links)</a></h3>





1. [Hangul Collation Requirements](http://www.open-std.org/jtc1/sc22/wg20/docs/n1037-Hangul%20Collation%20Requirements.htm)   
PS: There is the main source of information.



2. [Terminator weight for Hangul](http://code.activestate.com/lists/perl-unicode/2163/)



3. [Theory vs. practice for Korean text collation](http://blogs.msdn.com/b/michkap/archive/2005/02/25/380266.aspx)   
PS: there is no any practice. They do not the UCA :/



4. [Wiki](http://en.wikipedia.org/wiki/Unicode_collation_algorithm)



6. [Unicode implementer's guide part 3: Conjoining jamo behavior](http://useless-factor.blogspot.com/2007/08/unicode-implementers-guide-part-3.md)



7. [Unicode implementer's guide part 5: Collation](http://useless-factor.blogspot.com/2007/10/unicode-implementers-guide-part-5.md)



8. [Unicode collation works now](http://useless-factor.blogspot.com/2008/05/unicode-collation-works-now.md)   
PS: I found it so late. :(



9. [ICU](http://userguide.icu-project.org/collation/concepts)

10. [String Sorting (Natural) in Erlang Cookbook](http://trapexit.org/String_Sorting_%28Natural%29)
<pre>   FIXED: Combining character contractions. Apparently, two combining marks can
form a contraction. A straight reading of the UCA wouldn't predict
this, but not all of the UCA tests pass unless you check for
non-adjacent combining marks being in a contraction together, without
a noncombining mark to start it off.</pre>

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#blanked-2">blanked/2</a></td><td>Variable collation elements and any subsequent ignorables
are reset so that their weights at levels one through three are zero.</td></tr><tr><td valign="top"><a href="#compare-2">compare/2</a></td><td></td></tr><tr><td valign="top"><a href="#compare-3">compare/3</a></td><td></td></tr><tr><td valign="top"><a href="#ducet-1">ducet/1</a></td><td>In:  not reversed string.</td></tr><tr><td valign="top"><a href="#get_options-0">get_options/0</a></td><td></td></tr><tr><td valign="top"><a href="#get_options-1">get_options/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_options-2">get_options/2</a></td><td>If you want use this library without import *.hrl, you can create
a #uca_options {} record with this function.</td></tr><tr><td valign="top"><a href="#non_ignorable-2">non_ignorable/2</a></td><td>Variable collation elements are not reset to be ignorable, but
get the weights explicitly mentioned in the file.</td></tr><tr><td valign="top"><a href="#shift_trimmed-2">shift_trimmed/2</a></td><td>This option is the same as Shifted, except that all trailing
FFFFs are trimmed from the sort key.</td></tr><tr><td valign="top"><a href="#shifted-2">shifted/2</a></td><td>Variable collation elements are reset to zero at levels one through
three.</td></tr><tr><td valign="top"><a href="#sort-1">sort/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort-2">sort/2</a></td><td>Sort a string list.</td></tr><tr><td valign="top"><a href="#sort_array-1">sort_array/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort_array-2">sort_array/2</a></td><td></td></tr><tr><td valign="top"><a href="#sort_array_blanked-1">sort_array_blanked/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort_array_non_ignorable-1">sort_array_non_ignorable/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort_array_shift_trimmed-1">sort_array_shift_trimmed/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort_array_shifted-1">sort_array_shifted/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort_key-1">sort_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort_key-2">sort_key/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="blanked-2"></a>

<h3>blanked/2</h3>





`blanked(S1, S2) -> any()`



Variable collation elements and any subsequent ignorables
are reset so that their weights at levels one through three are zero.
For example,
<pre>  * SPACE would have the value [.0000.0000.0000]
* A combining grave accent after a space would have the value [.0000.0000.0000]
* Capital A would be unchanged, with the value [.06D9.0020.0008]
* A combining grave accent after a Capital A would be unchanged</pre><a name="compare-2"></a>

<h3>compare/2</h3>





`compare(String1, String2) -> any()`

<a name="compare-3"></a>

<h3>compare/3</h3>





`compare(String1, String2, Uca_options) -> any()`

<a name="ducet-1"></a>

<h3>ducet/1</h3>





`ducet(A) -> any()`



In:  not reversed string.
Out: not reversed weight list.<a name="get_options-0"></a>

<h3>get_options/0</h3>





`get_options() -> any()`

<a name="get_options-1"></a>

<h3>get_options/1</h3>





`get_options(Params) -> any()`

<a name="get_options-2"></a>

<h3>get_options/2</h3>





`get_options(T, Opt) -> any()`



If you want use this library without import *.hrl, you can create
a #uca_options {} record with this function.<a name="non_ignorable-2"></a>

<h3>non_ignorable/2</h3>





`non_ignorable(S1, S2) -> any()`



Variable collation elements are not reset to be ignorable, but
get the weights explicitly mentioned in the file.
<pre>  * SPACE would have the value [.0209.0020.0002]
* Capital A would be unchanged, with the value [.06D9.0020.0008]
* Ignorables are unchanged.</pre><a name="shift_trimmed-2"></a>

<h3>shift_trimmed/2</h3>





`shift_trimmed(S1, S2) -> any()`



This option is the same as Shifted, except that all trailing
FFFFs are trimmed from the sort key.
This could be used to emulate POSIX behavior.<a name="shifted-2"></a>

<h3>shifted/2</h3>





`shifted(S1, S2) -> any()`



Variable collation elements are reset to zero at levels one through
three. In addition, a new fourth-level weight is appended, whose value
depends on the type, as shown in Table 12.
Any subsequent primary or secondary ignorables following a variable are reset
so that their weights at levels one through four are zero.
<pre>  * A combining grave accent after a space would have the value
[.0000.0000.0000.0000].
* A combining grave accent after a Capital A would be unchanged.</pre><a name="sort-1"></a>

<h3>sort/1</h3>





`sort(Lists) -> any()`

<a name="sort-2"></a>

<h3>sort/2</h3>





`sort(Lists, Alt) -> any()`



Sort a string list.
Example:
<pre>      f().
RawData = ["death", "de luge", "de-luge", "deluge", "de-luge", "de Luge", "de-Luge", "deLuge", "de-Luge", "demark"].
Data = lists:map(fun lists:flatten/1, RawData).
ux_string:sort(Data, non_ignorable).
ux_string:sort(Data, blanked).
ux_string:sort(Data, shifted).
ux_string:sort(Data, shift_trimmed).</pre><a name="sort_array-1"></a>

<h3>sort_array/1</h3>





`sort_array(Str) -> any()`

<a name="sort_array-2"></a>

<h3>sort_array/2</h3>





`sort_array(Str, Params) -> any()`

<a name="sort_array_blanked-1"></a>

<h3>sort_array_blanked/1</h3>





`sort_array_blanked(Str) -> any()`

<a name="sort_array_non_ignorable-1"></a>

<h3>sort_array_non_ignorable/1</h3>





`sort_array_non_ignorable(Str) -> any()`

<a name="sort_array_shift_trimmed-1"></a>

<h3>sort_array_shift_trimmed/1</h3>





`sort_array_shift_trimmed(Str) -> any()`

<a name="sort_array_shifted-1"></a>

<h3>sort_array_shifted/1</h3>





`sort_array_shifted(Str) -> any()`

<a name="sort_key-1"></a>

<h3>sort_key/1</h3>





`sort_key(Str) -> any()`

<a name="sort_key-2"></a>

<h3>sort_key/2</h3>





`sort_key(Str, Uca_options) -> any()`

