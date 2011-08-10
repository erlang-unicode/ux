Module ux_uca
=============


<h1>Module ux_uca</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


UNICODE COLLATION ALGORITHM        
see Unicode Technical Standard #10.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).

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




For hangul:   
http://www.open-std.org/Jtc1/sc22/wg20/docs/n1037-Hangul%20Collation%20Requirements.htm   
http://www.unicode.org/reports/tr10/#Hangul_Collation   
http://en.wikipedia.org/wiki/KSX1001


Levels: http://unicode.org/reports/tr10/#Multi_Level_Comparison
<pre>   * L1 Base characters
* L2 Accents
* L3 Case
* L4 Punctuation</pre>

Example using levels:
<pre>   C = ux_uca_options:get_options([{strength, 3}]).
ux_uca:sort_key(C, "Get L1-L3 weights").</pre>




<h3><a name="Common_configurations">Common configurations</a></h3>





<h4><a name="Non-ignorable">Non-ignorable</a></h4>


Variable collation elements are not reset to be ignorable, but
get the weights explicitly mentioned in the file.
<pre>   * SPACE would have the value [.0209.0020.0002]
* Capital A would be unchanged, with the value [.06D9.0020.0008]
* Ignorables are unchanged.</pre>

Example:
<pre>   C = ux_uca_options:get_options(non_ignorable).
ux_uca:sort_key(C, "Non-ignorable collation sort key").</pre>




<h4><a name="Blanked">Blanked</a></h4>


Variable collation elements and any subsequent ignorables
are reset so that their weights at levels one through three are zero.
For example,
<pre>   * SPACE would have the value [.0000.0000.0000]
* A combining grave accent after a space would have the value
[.0000.0000.0000]
* Capital A would be unchanged, with the value [.06D9.0020.0008]
* A combining grave accent after a Capital A would be unchanged</pre>

Example:
<pre>   C = ux_uca_options:get_options(non_ignorable).
ux_uca:sort_key(C, "Blanked collation sort key").</pre>




<h4><a name="Shifted">Shifted</a></h4>


Variable collation elements are reset to zero at levels one through
three. In addition, a new fourth-level weight is appended, whose value
depends on the type, as shown in Table 12.
Any subsequent primary or secondary ignorables following a variable are reset
so that their weights at levels one through four are zero.
<pre>   * A combining grave accent after a space would have the value
[.0000.0000.0000.0000].
* A combining grave accent after a Capital A would be unchanged.</pre>




<h4><a name="Shift-trimmed">Shift-trimmed</a></h4>


This option is the same as Shifted, except that all trailing
FFFFs are trimmed from the sort key.
This could be used to emulate POSIX behavior.


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compare-2">compare/2</a></td><td></td></tr><tr><td valign="top"><a href="#compare-3">compare/3</a></td><td></td></tr><tr><td valign="top"><a href="#sort-1">sort/1</a></td><td>Sort a list of strings.</td></tr><tr><td valign="top"><a href="#sort-2">sort/2</a></td><td>Sort a list of strings.</td></tr><tr><td valign="top"><a href="#sort_array-1">sort_array/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort_array-2">sort_array/2</a></td><td></td></tr><tr><td valign="top"><a href="#sort_key-1">sort_key/1</a></td><td></td></tr><tr><td valign="top"><a href="#sort_key-2">sort_key/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="compare-2"></a>

<h3>compare/2</h3>





`compare(S1, S2) -> any()`

<a name="compare-3"></a>

<h3>compare/3</h3>





`compare(C, S1, S2) -> any()`

<a name="sort-1"></a>

<h3>sort/1</h3>





`sort(Strings) -> any()`



Sort a list of strings.<a name="sort-2"></a>

<h3>sort/2</h3>





`sort(C, Strings) -> any()`



Sort a list of strings.<a name="sort_array-1"></a>

<h3>sort_array/1</h3>





`sort_array(S) -> any()`

<a name="sort_array-2"></a>

<h3>sort_array/2</h3>





`sort_array(C, S) -> any()`

<a name="sort_key-1"></a>

<h3>sort_key/1</h3>





`sort_key(S) -> any()`

<a name="sort_key-2"></a>

<h3>sort_key/2</h3>





`sort_key(C, S) -> any()`

