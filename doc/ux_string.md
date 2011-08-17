Module ux_string
================


<h1>Module ux_string</h1>

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


String functions.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`freeakk@gmail.com`](mailto:freeakk@gmail.com)).

__See also:__ [ux](ux.md).

<h2><a name="index">Function Index</a></h2>



<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete_types-2">delete_types/2</a></td><td>Returns a new string which is made from the chars of Str
which are not a type from Types list.</td></tr><tr><td valign="top"><a href="#delete_types-3">delete_types/3</a></td><td>Stops delete_type/2 after Limit deleted chars.</td></tr><tr><td valign="top"><a href="#explode-2">explode/2</a></td><td>Splits the string by delimeters.</td></tr><tr><td valign="top"><a href="#explode-3">explode/3</a></td><td></td></tr><tr><td valign="top"><a href="#explode_types-2">explode_types/2</a></td><td>Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list.</td></tr><tr><td valign="top"><a href="#filter_types-2">filter_types/2</a></td><td>Returns a new string which is made from the chars of Str
which are a type from Types list.</td></tr><tr><td valign="top"><a href="#filter_types-3">filter_types/3</a></td><td>Stops after -Limit skipped chars.</td></tr><tr><td valign="top"><a href="#first-2">first/2</a></td><td>Return Len chars from the beginning of the string.</td></tr><tr><td valign="top"><a href="#first_types-3">first_types/3</a></td><td>If Len<0, then gets first Len chars of type, which is in Types
If Len>0, then gets first -Len chars of type, which is NOT in Types.</td></tr><tr><td valign="top"><a href="#freq-1">freq/1</a></td><td>Counts how many identical chars in the string.</td></tr><tr><td valign="top"><a href=".md_special_chars-1">html_special_chars/1</a></td><td>Encodes html special chars.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Return information about a string.</td></tr><tr><td valign="top"><a href="#is_nfc-1">is_nfc/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_nfd-1">is_nfd/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_nfkc-1">is_nfkc/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_nfkd-1">is_nfkd/1</a></td><td></td></tr><tr><td valign="top"><a href="#last-2">last/2</a></td><td>Return Len chars from the beginning of the string.</td></tr><tr><td valign="top"><a href="#last_types-3">last_types/3</a></td><td>If Len<0, then gets last Len chars of type, which is in Types
If Len>0, then gets last -Len chars of type, which is NOT in Types.</td></tr><tr><td valign="top"><a href="#length-1">length/1</a></td><td>Compute count of graphemes in the string.</td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td>Reverses the string graphemes.</td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td></td></tr><tr><td valign="top"><a href="#split-3">split/3</a></td><td></td></tr><tr><td valign="top"><a href="#split_types-2">split_types/2</a></td><td>Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list.</td></tr><tr><td valign="top"><a href="#strip_tags-1">strip_tags/1</a></td><td>Deletes tags from the string.</td></tr><tr><td valign="top"><a href="#strip_tags-2">strip_tags/2</a></td><td></td></tr><tr><td valign="top"><a href="#strip_tags-3">strip_tags/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_graphemes-1">to_graphemes/1</a></td><td>Split unicode string into  
[graphemes](http://en.wikipedia.org/wiki/Grapheme).</td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td>Converts characters of a string to a lowercase format.</td></tr><tr><td valign="top"><a href="#to_ncr-1">to_ncr/1</a></td><td>Convert everything from utf-8 into an NCR (Numeric Character Reference).</td></tr><tr><td valign="top"><a href="#to_nfc-1">to_nfc/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfd-1">to_nfd/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfkc-1">to_nfkc/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfkd-1">to_nfkd/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_string-1">to_string/1</a></td><td>Converts something to string (list).</td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td>Converts characters of a string to a uppercase format.</td></tr><tr><td valign="top"><a href="#types-1">types/1</a></td><td>Returns various "character types" which can be used
as a default categorization in implementations.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="delete_types-2"></a>

<h3>delete_types/2</h3>





<pre>delete_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string()) -> string() | none()</pre>
<br></br>




Returns a new string which is made from the chars of Str
which are not a type from Types list.<a name="delete_types-3"></a>

<h3>delete_types/3</h3>





<pre>delete_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string(), Limit::integer()) -> string() | none()</pre>
<br></br>




Stops delete_type/2 after Limit deleted chars. If Limit < 0, then
stops after -Limit skipped chars.<a name="explode-2"></a>

<h3>explode/2</h3>





<pre>explode(Delimeter::[string()], Str::string()) -> [string()]</pre>
<br></br>




Splits the string by delimeters.<a name="explode-3"></a>

<h3>explode/3</h3>





<pre>explode(Delimeter::[string()], Str::string(), Limit::integer()) -> string()</pre>
<br></br>


<a name="explode_types-2"></a>

<h3>explode_types/2</h3>





<pre>explode_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string()) -> string()</pre>
<br></br>




Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list.<a name="filter_types-2"></a>

<h3>filter_types/2</h3>





<pre>filter_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string()) -> string() | none()</pre>
<br></br>




Returns a new string which is made from the chars of Str
which are a type from Types list.<a name="filter_types-3"></a>

<h3>filter_types/3</h3>





<pre>filter_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string(), Limit::integer()) -> string() | none()</pre>
<br></br>




Stops after -Limit skipped chars.<a name="first-2"></a>

<h3>first/2</h3>





`first(Str, Len) -> any()`



Return Len chars from the beginning of the string.<a name="first_types-3"></a>

<h3>first_types/3</h3>





<pre>first_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string(), Len::integer()) -> string() | none()</pre>
<br></br>




If Len<0, then gets first Len chars of type, which is in Types
If Len>0, then gets first -Len chars of type, which is NOT in Types<a name="freq-1"></a>

<h3>freq/1</h3>





<pre>freq(Str::string()) -> dict()</pre>
<br></br>




Counts how many identical chars in the string.
Returns a dict.
Example:
<pre>  >dict:to_list(ux_string:freq("FFDF")).
  [{70,3},{68,1}]</pre>
<a name="html_special_chars-1"></a>

<h3>html_special_chars/1</h3>





<pre>html_special_chars(Str::string()) -> string()</pre>
<br></br>




Encodes html special chars.<a name="info-1"></a>

<h3>info/1</h3>





<pre>info(Str::string()) -> #unistr_info{}</pre>
<br></br>




Return information about a string.<a name="is_nfc-1"></a>

<h3>is_nfc/1</h3>





<pre>is_nfc(Str::list()) -> yes | no | maybe</pre>
<br></br>


<a name="is_nfd-1"></a>

<h3>is_nfd/1</h3>





<pre>is_nfd(Str::list()) -> yes | no | maybe</pre>
<br></br>


<a name="is_nfkc-1"></a>

<h3>is_nfkc/1</h3>





<pre>is_nfkc(Str::list()) -> yes | no | maybe</pre>
<br></br>


<a name="is_nfkd-1"></a>

<h3>is_nfkd/1</h3>





<pre>is_nfkd(Str::list()) -> yes | no | maybe</pre>
<br></br>


<a name="last-2"></a>

<h3>last/2</h3>





`last(Str, Len) -> any()`



Return Len chars from the beginning of the string.<a name="last_types-3"></a>

<h3>last_types/3</h3>





<pre>last_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string(), Len::integer()) -> string() | none()</pre>
<br></br>




If Len<0, then gets last Len chars of type, which is in Types
If Len>0, then gets last -Len chars of type, which is NOT in Types<a name="length-1"></a>

<h3>length/1</h3>





`length(Str) -> any()`



Compute count of graphemes in the string.<a name="reverse-1"></a>

<h3>reverse/1</h3>





`reverse(Str) -> any()`



Reverses the string graphemes.<a name="split-2"></a>

<h3>split/2</h3>





`split(P1, P2) -> any()`

<a name="split-3"></a>

<h3>split/3</h3>





`split(P1, P2, P3) -> any()`

<a name="split_types-2"></a>

<h3>split_types/2</h3>





<pre>split_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string()) -> string()</pre>
<br></br>




Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list. Parts can not be
empty.<a name="strip_tags-1"></a>

<h3>strip_tags/1</h3>





<pre>strip_tags(Str::string()) -> string()</pre>
<br></br>






Deletes tags from the string.

Example:
<pre>       > ux_string:strip_tags("<b>some string</b>").
       "some string"
       > ux_string:strip_tags("<h1>Head</h1><p>and paragraf</p>", ["h1"]).
       "<h1>Head</h1>and paragraf"
       ux_string:strip_tags("<h1>Head</h1><p><!-- and paragraf --></p>", ["!--"]).
       "Head<!-- and paragraf -->"
       ux_string:st("a<br />b", [], " ").
       "a b"</pre><a name="strip_tags-2"></a>

<h3>strip_tags/2</h3>





<pre>strip_tags(Str::string, Allowed::[string() | atom() | char()]) -> string()</pre>
<br></br>


<a name="strip_tags-3"></a>

<h3>strip_tags/3</h3>





<pre>strip_tags(Str::string, Allowed::[string() | atom() | char()], Alt::string()) -> string()</pre>
<br></br>


<a name="to_graphemes-1"></a>

<h3>to_graphemes/1</h3>





`to_graphemes(Str) -> any()`





Split unicode string into  
[graphemes](http://en.wikipedia.org/wiki/Grapheme).  
Based on  
[UAX29: UNICODE TEXT SEGMENTATION]  
(http://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries).

It is important to recognize that what the user thinks of as
a "character"—a basic unit of a writing system for a language—may
not be just a single Unicode code point. Instead, that basic unit
may be made up of multiple Unicode code points.
To avoid ambiguity  with the computer use of the term character,
this is called a user-perceived character.
For example, “G” + acute-accent is a user-perceived character:
users think of it as a single character, yet is actually represented
by two Unicode code points. These user-perceived characters are
approximated by what is called a grapheme cluster, which can be
determined programmatically.<a name="to_lower-1"></a>

<h3>to_lower/1</h3>





<pre>to_lower(Str::string()) -> string()</pre>
<br></br>




Converts characters of a string to a lowercase format.<a name="to_ncr-1"></a>

<h3>to_ncr/1</h3>





`to_ncr(Str) -> any()`



Convert everything from utf-8 into an NCR (Numeric Character Reference).<a name="to_nfc-1"></a>

<h3>to_nfc/1</h3>





<pre>to_nfc(Str::list()) -> list()</pre>
<br></br>


<a name="to_nfd-1"></a>

<h3>to_nfd/1</h3>





<pre>to_nfd(Str::list()) -> list()</pre>
<br></br>


<a name="to_nfkc-1"></a>

<h3>to_nfkc/1</h3>





<pre>to_nfkc(Str::list()) -> list()</pre>
<br></br>


<a name="to_nfkd-1"></a>

<h3>to_nfkd/1</h3>





<pre>to_nfkd(Str::list()) -> list()</pre>
<br></br>


<a name="to_string-1"></a>

<h3>to_string/1</h3>





<pre>to_string(Str::string() | atom() | integer()) -> string()</pre>
<br></br>




Converts something to string (list).<a name="to_upper-1"></a>

<h3>to_upper/1</h3>





<pre>to_upper(Str::string()) -> string()</pre>
<br></br>




Converts characters of a string to a uppercase format.<a name="types-1"></a>

<h3>types/1</h3>





`types(Str) -> any()`



Returns various "character types" which can be used
as a default categorization in implementations.
Types:
http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#General%20Category