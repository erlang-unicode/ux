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
If Len>0, then gets first -Len chars of type, which is NOT in Types.</td></tr><tr><td valign="top"><a href="#freq-1">freq/1</a></td><td>Counts a letter frequency.</td></tr><tr><td valign="top"><a href="#hex_to_int-1">hex_to_int/1</a></td><td></td></tr><tr><td valign="top"><a href=".md_special_chars-1">html_special_chars/1</a></td><td>Encodes html special chars.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td>Return information about a string.</td></tr><tr><td valign="top"><a href="#is_nfc-1">is_nfc/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_nfd-1">is_nfd/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_nfkc-1">is_nfkc/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_nfkd-1">is_nfkd/1</a></td><td></td></tr><tr><td valign="top"><a href="#last-2">last/2</a></td><td>Return Len chars from the beginning of the string.</td></tr><tr><td valign="top"><a href="#last_types-3">last_types/3</a></td><td>If Len<0, then gets last Len chars of type, which is in Types
If Len>0, then gets last -Len chars of type, which is NOT in Types.</td></tr><tr><td valign="top"><a href="#len-1">len/1</a></td><td></td></tr><tr><td valign="top"><a href="#length-1">length/1</a></td><td>Compute count of graphemes in the string.</td></tr><tr><td valign="top"><a href="#list_to_latin1-1">list_to_latin1/1</a></td><td></td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td>Reverses string graphemes.</td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td></td></tr><tr><td valign="top"><a href="#split-3">split/3</a></td><td></td></tr><tr><td valign="top"><a href="#split_types-2">split_types/2</a></td><td>Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list.</td></tr><tr><td valign="top"><a href="#strip_tags-1">strip_tags/1</a></td><td>Deletes tags from the string.</td></tr><tr><td valign="top"><a href="#strip_tags-2">strip_tags/2</a></td><td></td></tr><tr><td valign="top"><a href="#strip_tags-3">strip_tags/3</a></td><td></td></tr><tr><td valign="top"><a href="#to_graphemes-1">to_graphemes/1</a></td><td>Split unicode string into
[graphemes](http://en.wikipedia.org/wiki/Grapheme).</td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td>Converts characters of a string to a lowercase format.</td></tr><tr><td valign="top"><a href="#to_ncr-1">to_ncr/1</a></td><td>Convert everything from utf-8 into an NCR (Numeric Character Reference).</td></tr><tr><td valign="top"><a href="#to_nfc-1">to_nfc/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfd-1">to_nfd/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfkc-1">to_nfkc/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfkd-1">to_nfkd/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_string-1">to_string/1</a></td><td>Converts something to string (list).</td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td>Converts characters of a string to a uppercase format.</td></tr><tr><td valign="top"><a href="#types-1">types/1</a></td><td>Returns various "character types" which can be used
as a default categorization in implementations.</td></tr></table>




<h2><a name="functions">Function Details</a></h2>


<a name="delete_types-2"></a>

<h3>delete_types/2</h3>





`delete_types(Types, Str) -> any()`



Returns a new string which is made from the chars of Str
which are not a type from Types list.<a name="delete_types-3"></a>

<h3>delete_types/3</h3>





`delete_types(Types, Str, Limit) -> any()`



Stops delete_type/2 after Limit deleted chars. If Limit < 0, then
stops after -Limit skipped chars.<a name="explode-2"></a>

<h3>explode/2</h3>





`explode(Delimeter, Str) -> any()`



Splits the string by delimeters.<a name="explode-3"></a>

<h3>explode/3</h3>





`explode(Delimeter, Str, Limit) -> any()`

<a name="explode_types-2"></a>

<h3>explode_types/2</h3>





`explode_types(Types, Str) -> any()`



Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list.<a name="filter_types-2"></a>

<h3>filter_types/2</h3>





`filter_types(Types, Str) -> any()`



Returns a new string which is made from the chars of Str
which are a type from Types list.<a name="filter_types-3"></a>

<h3>filter_types/3</h3>





`filter_types(Types, Str, Limit) -> any()`



Stops after -Limit skipped chars.<a name="first-2"></a>

<h3>first/2</h3>





`first(Str, Len) -> any()`



Return Len chars from the beginning of the string.<a name="first_types-3"></a>

<h3>first_types/3</h3>





`first_types(Types, Str, Len) -> any()`



If Len<0, then gets first Len chars of type, which is in Types
If Len>0, then gets first -Len chars of type, which is NOT in Types<a name="freq-1"></a>

<h3>freq/1</h3>





`freq(Str) -> any()`



Counts a letter frequency<a name="hex_to_int-1"></a>

<h3>hex_to_int/1</h3>





`hex_to_int(Code) -> any()`

<a name="html_special_chars-1"></a>

<h3>html_special_chars/1</h3>





`html_special_chars(Str) -> any()`



Encodes html special chars.<a name="info-1"></a>

<h3>info/1</h3>





`info(Rec) -> any()`



Return information about a string.<a name="is_nfc-1"></a>

<h3>is_nfc/1</h3>





`is_nfc(Str) -> any()`

<a name="is_nfd-1"></a>

<h3>is_nfd/1</h3>





`is_nfd(Str) -> any()`

<a name="is_nfkc-1"></a>

<h3>is_nfkc/1</h3>





`is_nfkc(Str) -> any()`

<a name="is_nfkd-1"></a>

<h3>is_nfkd/1</h3>





`is_nfkd(Str) -> any()`

<a name="last-2"></a>

<h3>last/2</h3>





`last(Str, Len) -> any()`



Return Len chars from the beginning of the string.<a name="last_types-3"></a>

<h3>last_types/3</h3>





`last_types(Types, Str, Len) -> any()`



If Len<0, then gets last Len chars of type, which is in Types
If Len>0, then gets last -Len chars of type, which is NOT in Types<a name="len-1"></a>

<h3>len/1</h3>





`len(Str) -> any()`

<a name="length-1"></a>

<h3>length/1</h3>





`length(Str) -> any()`



Compute count of graphemes in the string.<a name="list_to_latin1-1"></a>

<h3>list_to_latin1/1</h3>





`list_to_latin1(Str) -> any()`

<a name="reverse-1"></a>

<h3>reverse/1</h3>





`reverse(Str) -> any()`



Reverses string graphemes.<a name="split-2"></a>

<h3>split/2</h3>





`split(P1, P2) -> any()`

<a name="split-3"></a>

<h3>split/3</h3>





`split(P1, P2, P3) -> any()`

<a name="split_types-2"></a>

<h3>split_types/2</h3>





`split_types(Types, Str) -> any()`



Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list. Parts can not be
empty.<a name="strip_tags-1"></a>

<h3>strip_tags/1</h3>





`strip_tags(Str) -> any()`





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





`strip_tags(Str, Allowed) -> any()`

<a name="strip_tags-3"></a>

<h3>strip_tags/3</h3>





`strip_tags(Str, Allowed, Alt) -> any()`

<a name="to_graphemes-1"></a>

<h3>to_graphemes/1</h3>





`to_graphemes(Str) -> any()`



Split unicode string into
[graphemes](http://en.wikipedia.org/wiki/Grapheme)<a name="to_lower-1"></a>

<h3>to_lower/1</h3>





`to_lower(Str) -> any()`



Converts characters of a string to a lowercase format.<a name="to_ncr-1"></a>

<h3>to_ncr/1</h3>





`to_ncr(Str) -> any()`



Convert everything from utf-8 into an NCR (Numeric Character Reference).<a name="to_nfc-1"></a>

<h3>to_nfc/1</h3>





`to_nfc(Str) -> any()`

<a name="to_nfd-1"></a>

<h3>to_nfd/1</h3>





`to_nfd(Str) -> any()`

<a name="to_nfkc-1"></a>

<h3>to_nfkc/1</h3>





`to_nfkc(Str) -> any()`

<a name="to_nfkd-1"></a>

<h3>to_nfkd/1</h3>





`to_nfkd(Str) -> any()`

<a name="to_string-1"></a>

<h3>to_string/1</h3>





`to_string(Str) -> any()`



Converts something to string (list).<a name="to_upper-1"></a>

<h3>to_upper/1</h3>





`to_upper(Str) -> any()`



Converts characters of a string to a uppercase format.<a name="types-1"></a>

<h3>types/1</h3>





`types(Str) -> any()`



Returns various "character types" which can be used
as a default categorization in implementations.
Types:
http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#General%20Category