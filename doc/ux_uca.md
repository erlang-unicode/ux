

#Module ux_uca#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


UNICODE COLLATION ALGORITHM        
see Unicode Technical Standard #10.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).<a name="description"></a>

##Description##




###<a name="Additional_information_(and_links)">Additional information (and links)</a>##




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




###<a name="Levels">Levels</a>##


   
http://unicode.org/reports/tr10/#Multi_Level_Comparison



* L1 Base characters   
* L2 Accents   
* L3 Case   
* L4 Punctuation

Example using levels:
<pre>   C = ux_uca_options:get_options([{strength, 3}]).
   ux_uca:sort_key(C, "Get L1-L3 weights").</pre>




###<a name="Common_configurations">Common configurations</a>##




####<a name="Non-ignorable">Non-ignorable</a>##


   
Variable collation elements are not reset to be ignorable, but   
get the weights explicitly mentioned in the file.



* SPACE would have the value [.0209.0020.0002]   
* Capital A would be unchanged, with the value [.06D9.0020.0008]   
* Ignorables are unchanged.

Example:
<pre>   C = ux_uca_options:get_options(non_ignorable).
   ux_uca:sort_key(C, "Non-ignorable collation sort key").</pre>




####<a name="Blanked">Blanked</a>##


   
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




####<a name="Shifted">Shifted</a>##


   
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




####<a name="Shift-trimmed">Shift-trimmed</a>##


   
This option is the same as Shifted, except that all trailing   
FFFFs are trimmed from the sort key.   
This could be used to emulate POSIX behavior.

Example:
<pre>   C = ux_uca_options:get_options(shift_trimmed).
   ux_uca:sort_key(C, "Shift-trimmed collation sort key").</pre>


<a name="types"></a>

##Data Types##




###<a name="type-result">result()</a>##



<pre>result() = {[<a href="#type-uca_elem">uca_elem()</a>], string()}</pre>



###<a name="type-search_result">search_result()</a>##



<pre>search_result() = {string(), string(), string()}</pre>



###<a name="type-uca_alternate">uca_alternate()</a>##



<pre>uca_alternate() = shifted | shift_trimmed | non_ignorable | blanked</pre>



###<a name="type-uca_array">uca_array()</a>##



<pre>uca_array() = [<a href="#type-uca_elem">uca_elem()</a>]</pre>



###<a name="type-uca_case_first">uca_case_first()</a>##



<pre>uca_case_first() = lower | upper | off</pre>



###<a name="type-uca_compare_result">uca_compare_result()</a>##



<pre>uca_compare_result() = lower | greater | equal</pre>



###<a name="type-uca_elem">uca_elem()</a>##



<pre>uca_elem() = [atom() | <a href="#type-uca_weight">uca_weight()</a>]</pre>



###<a name="type-uca_sort_key_format">uca_sort_key_format()</a>##



<pre>uca_sort_key_format() = binary | list | uncompressed</pre>



###<a name="type-uca_strength">uca_strength()</a>##



<pre>uca_strength() = 1 | 2 | 3 | 4</pre>



###<a name="type-uca_weight">uca_weight()</a>##



<pre>uca_weight() = integer()</pre>



###<a name="type-uca_weights">uca_weights()</a>##



<pre>uca_weights() = [<a href="#type-uca_weight">uca_weight()</a>]</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#compare-2">compare/2</a></td><td>Compare two strings and return: lower, greater or equal.</td></tr><tr><td valign="top"><a href="#compare-3">compare/3</a></td><td></td></tr><tr><td valign="top"><a href="#search-2">search/2</a></td><td></td></tr><tr><td valign="top"><a href="#search-3">search/3</a></td><td></td></tr><tr><td valign="top"><a href="#search-4">search/4</a></td><td></td></tr><tr><td valign="top"><a href="#sort-1">sort/1</a></td><td>Sort a list of strings.</td></tr><tr><td valign="top"><a href="#sort-2">sort/2</a></td><td>Sort a list of strings.</td></tr><tr><td valign="top"><a href="#sort_array-1">sort_array/1</a></td><td>Convert the unicode string to the
<a href="http://unicode.org/reports/tr10/#Step_2" target="_top">collation element array</a></td></tr><tr><td valign="top"><a href="#sort_array-2">sort_array/2</a></td><td></td></tr><tr><td valign="top"><a href="#sort_key-1">sort_key/1</a></td><td>Convert the unicode string to the sort key.</td></tr><tr><td valign="top"><a href="#sort_key-2">sort_key/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="compare-2"></a>

###compare/2##




<pre>compare(S1::string(), S2::string()) -> <a href="#type-uca_compare_result">uca_compare_result()</a></pre>
<br></br>




Compare two strings and return: lower, greater or equal.<a name="compare-3"></a>

###compare/3##




<pre>compare(Uca_options::#uca_options{}, S1::string(), S2::string()) -> <a href="#type-uca_compare_result">uca_compare_result()</a></pre>
<br></br>


<a name="search-2"></a>

###search/2##




<pre>search(Target::string(), Pattern::string()) -> <a href="#type-search_result">search_result()</a></pre>
<br></br>


<a name="search-3"></a>

###search/3##




<pre>search(Target::string(), Pattern::string(), MatchStyle::atom()) -> <a href="#type-search_result">search_result()</a></pre>
<br></br>


<a name="search-4"></a>

###search/4##




<pre>search(Uca_options::#uca_options{}, Target::string(), Pattern::string(), MatchStyle::atom()) -> <a href="#type-search_result">search_result()</a></pre>
<br></br>


<a name="sort-1"></a>

###sort/1##




<pre>sort(Strings::[string()]) -&gt; [string()]</pre>
<br></br>




Sort a list of strings.<a name="sort-2"></a>

###sort/2##




<pre>sort(Uca_options::#uca_options{}, Strings::[string()]) -&gt; [string()]</pre>
<br></br>




Sort a list of strings.<a name="sort_array-1"></a>

###sort_array/1##




`sort_array(S) -> any()`



Convert the unicode string to the
[collation element array](http://unicode.org/reports/tr10/#Step_2)<a name="sort_array-2"></a>

###sort_array/2##




`sort_array(C, S) -> any()`

<a name="sort_key-1"></a>

###sort_key/1##




`sort_key(S) -> any()`



Convert the unicode string to the sort key.<a name="sort_key-2"></a>

###sort_key/2##




`sort_key(C, S) -> any()`

