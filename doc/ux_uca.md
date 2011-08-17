Module ux_uca
=============


<h1>Module ux_uca</h1>

* [Description](#description)
* [Data Types](#types)
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




For hangul collation:
11. [Hangul Collation Requirements](http://www.open-std.org/Jtc1/sc22/wg20/docs/n1037-Hangul%20Collation%20Requirements.htm)
12. [UTR 10](http://www.unicode.org/reports/tr10/#Hangul_Collation)
13. [KSX1001 on Wiki](http://en.wikipedia.org/wiki/KSX1001)




<h3><a name="Levels">Levels</a></h3>



   
http://unicode.org/reports/tr10/#Multi_Level_Comparison



* L1 Base characters   
* L2 Accents   
* L3 Case   
* L4 Punctuation

Example using levels:
<pre>   C = ux_uca_options:get_options([{strength, 3}]).
   ux_uca:sort_key(C, "Get L1-L3 weights").</pre>




<h3><a name="Common_configurations">Common configurations</a></h3>





<h4><a name="Non-ignorable">Non-ignorable</a></h4>



   
Variable collation elements are not reset to be ignorable, but   
get the weights explicitly mentioned in the file.



* SPACE would have the value [.0209.0020.0002]   
* Capital A would be unchanged, with the value [.06D9.0020.0008]   
* Ignorables are unchanged.

Example:
<pre>   C = ux_uca_options:get_options(non_ignorable).
   ux_uca:sort_key(C, "Non-ignorable collation sort key").</pre>




<h4><a name="Blanked">Blanked</a></h4>



   
Variable collation elements and any subsequent ignorables   
are reset so that their weights at levels one through three are zero.   
For example,



* SPACE would have the value [.0000.0000.0000]   
* A combining grave accent after a space would have the value     
[.0000.0000.0000]   
* Capital A would be unchanged, with the value [.06D9.0020.0008]   
* A combining grave accent after a Capital A would be unchanged

Example:
<pre>   C = ux_uca_options:get_options(non_ignorable).
   ux_uca:sort_key(C, "Blanked collation sort key").</pre>




<h4><a name="Shifted">Shifted</a></h4>



   
Variable collation elements are reset to zero at levels one through   
three. In addition, a new fourth-level weight is appended, whose value   
depends on the type, as shown in Table 12.   
Any subsequent primary or secondary ignorables following a variable are reset   
so that their weights at levels one through four are zero.



* A combining grave accent after a space would have the value     
[.0000.0000.0000.0000].   
* A combining grave accent after a Capital A would be unchanged.

Example:
<pre>   C = ux_uca_options:get_options(shifted).
   ux_uca:sort_key(C, "Shifted collation sort key").</pre>




<h4><a name="Shift-trimmed">Shift-trimmed</a></h4>



   
This option is the same as Shifted, except that all trailing   
FFFFs are trimmed from the sort key.   
This could be used to emulate POSIX behavior.

Example:
<pre>   C = ux_uca_options:get_options(shift_trimmed).
   ux_uca:sort_key(C, "Shift-trimmed collation sort key").</pre>




<h2><a name="types">Data Types</a></h2>





<h3 class="typedecl"><a name="type-search_result">search_result()</a></h3>




<pre>search_result() = {string(), string(), string()}</pre>



<h3 class="typedecl"><a name="type-uca_compare_result">uca_compare_result()</a></h3>




<pre>uca_compare_result() = lower | greater | equal</pre>


<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compare-2">compare/2</a></td><td>Compare two strings and return: lower, greater or equal.</td></tr><tr><td valign="top"><a href="#compare-3">compare/3</a></td><td></td></tr><tr><td valign="top"><a href="#search-2">search/2</a></td><td></td></tr><tr><td valign="top"><a href="#search-3">search/3</a></td><td></td></tr><tr><td valign="top"><a href="#search-4">search/4</a></td><td></td></tr><tr><td valign="top"><a href="#sort-1">sort/1</a></td><td>Sort a list of strings.</td></tr><tr><td valign="top"><a href="#sort-2">sort/2</a></td><td>Sort a list of strings.</td></tr><tr><td valign="top"><a href="#sort_array-1">sort_array/1</a></td><td>Convert the unicode string to the
<a href="http://unicode.org/reports/tr10/#Step_2" target="_top">collation element array</a></td></tr><tr><td valign="top"><a href="#sort_array-2">sort_array/2</a></td><td></td></tr><tr><td valign="top"><a href="#sort_key-1">sort_key/1</a></td><td>Convert the unicode string to the sort key.</td></tr><tr><td valign="top"><a href="#sort_key-2">sort_key/2</a></td><td></td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="compare-2"></a>

<h3>compare/2</h3>





<pre>compare(S1::string(), S2::string()) -> <a href="#type-uca_compare_result">uca_compare_result()</a></pre>
<br></br>




Compare two strings and return: lower, greater or equal.<a name="compare-3"></a>

<h3>compare/3</h3>





<pre>compare(Uca_options::#uca_options{}, S1::string(), S2::string()) -> <a href="#type-uca_compare_result">uca_compare_result()</a></pre>
<br></br>


<a name="search-2"></a>

<h3>search/2</h3>





<pre>search(Target::string(), Pattern::string()) -> <a href="#type-search_result">search_result()</a></pre>
<br></br>


<a name="search-3"></a>

<h3>search/3</h3>





<pre>search(Target::string(), Pattern::string(), MatchStyle::atom()) -> <a href="#type-search_result">search_result()</a></pre>
<br></br>


<a name="search-4"></a>

<h3>search/4</h3>





<pre>search(Uca_options::#uca_options{}, Target::string(), Pattern::string(), MatchStyle::atom()) -> <a href="#type-search_result">search_result()</a></pre>
<br></br>


<a name="sort-1"></a>

<h3>sort/1</h3>





<pre>sort(Strings::[string()]) -> [string()]</pre>
<br></br>




Sort a list of strings.<a name="sort-2"></a>

<h3>sort/2</h3>





<pre>sort(Uca_options::#uca_options{}, Strings::[string()]) -> [string()]</pre>
<br></br>




Sort a list of strings.<a name="sort_array-1"></a>

<h3>sort_array/1</h3>





`sort_array(S) -> any()`



Convert the unicode string to the
[collation element array](http://unicode.org/reports/tr10/#Step_2)<a name="sort_array-2"></a>

<h3>sort_array/2</h3>





`sort_array(C, S) -> any()`

<a name="sort_key-1"></a>

<h3>sort_key/1</h3>





`sort_key(S) -> any()`



Convert the unicode string to the sort key.<a name="sort_key-2"></a>

<h3>sort_key/2</h3>





`sort_key(C, S) -> any()`

