

#Module ux_string#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


String functions.



Copyright (c) 2010-2011 Michael Uvarov

__Authors:__ Michael Uvarov ([`arcusfelis@gmail.com`](mailto:arcusfelis@gmail.com)).
<a name="types"></a>

##Data Types##




###<a name="type-char_type">char_type()</a>##



<pre>char_type() = <a href="ux_types.md#type-char_type">ux_types:char_type()</a></pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete_types-2">delete_types/2</a></td><td>Returns a new string which is made from the chars of Str
which are not a type from Types list.</td></tr><tr><td valign="top"><a href="#delete_types-3">delete_types/3</a></td><td>Stops delete_type/2 after Limit deleted chars.</td></tr><tr><td valign="top"><a href="#explode-2">explode/2</a></td><td>Splits the string by delimeters.</td></tr><tr><td valign="top"><a href="#explode-3">explode/3</a></td><td></td></tr><tr><td valign="top"><a href="#explode_types-2">explode_types/2</a></td><td>Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list.</td></tr><tr><td valign="top"><a href="#extract_words-1">extract_words/1</a></td><td></td></tr><tr><td valign="top"><a href="#filter_types-2">filter_types/2</a></td><td>Returns a new string which is made from the chars of Str
which are a type from Types list.</td></tr><tr><td valign="top"><a href="#filter_types-3">filter_types/3</a></td><td>Stops after -Limit skipped chars.</td></tr><tr><td valign="top"><a href="#first-2">first/2</a></td><td>Return Len chars from the beginning of the string.</td></tr><tr><td valign="top"><a href="#first_types-3">first_types/3</a></td><td>If Len<0, then gets first Len chars of type, which is in Types
If Len>0, then gets first -Len chars of type, which is NOT in Types.</td></tr><tr><td valign="top"><a href="#freq-1">freq/1</a></td><td>Counts how many identical chars in the string.</td></tr><tr><td valign="top"><a href="#is_nfc-1">is_nfc/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_nfd-1">is_nfd/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_nfkc-1">is_nfkc/1</a></td><td></td></tr><tr><td valign="top"><a href="#is_nfkd-1">is_nfkd/1</a></td><td></td></tr><tr><td valign="top"><a href="#last-2">last/2</a></td><td>Return Len chars from the beginning of the string.</td></tr><tr><td valign="top"><a href="#last_types-3">last_types/3</a></td><td>If Len<0, then gets last Len chars of type, which is in Types
If Len>0, then gets last -Len chars of type, which is NOT in Types.</td></tr><tr><td valign="top"><a href="#length-1">length/1</a></td><td>Compute count of graphemes in the string.</td></tr><tr><td valign="top"><a href="#reverse-1">reverse/1</a></td><td>Reverses the string graphemes.</td></tr><tr><td valign="top"><a href="#script-1">script/1</a></td><td></td></tr><tr><td valign="top"><a href="#scripts-1">scripts/1</a></td><td></td></tr><tr><td valign="top"><a href="#split-2">split/2</a></td><td></td></tr><tr><td valign="top"><a href="#split-3">split/3</a></td><td></td></tr><tr><td valign="top"><a href="#split_types-2">split_types/2</a></td><td>Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list.</td></tr><tr><td valign="top"><a href="#to_graphemes-1">to_graphemes/1</a></td><td>Split unicode string into  
[graphemes](http://en.wikipedia.org/wiki/Grapheme).</td></tr><tr><td valign="top"><a href="#to_lower-1">to_lower/1</a></td><td>Converts characters of a string to a lowercase format.</td></tr><tr><td valign="top"><a href="#to_ncr-1">to_ncr/1</a></td><td>Convert everything from utf-8 into an NCR (Numeric Character Reference).</td></tr><tr><td valign="top"><a href="#to_nfc-1">to_nfc/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfd-1">to_nfd/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfkc-1">to_nfkc/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_nfkd-1">to_nfkd/1</a></td><td></td></tr><tr><td valign="top"><a href="#to_string-1">to_string/1</a></td><td>Converts something to string (list).</td></tr><tr><td valign="top"><a href="#to_upper-1">to_upper/1</a></td><td>Converts characters of a string to a uppercase format.</td></tr><tr><td valign="top"><a href="#types-1">types/1</a></td><td>Returns various "character types" which can be used
as a default categorization in implementations.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="delete_types-2"></a>

###delete_types/2##




<pre>delete_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string()) -> string() | none()</pre>
<br></br>




Returns a new string which is made from the chars of Str
which are not a type from Types list.<a name="delete_types-3"></a>

###delete_types/3##




<pre>delete_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string(), Limit::integer()) -> string() | none()</pre>
<br></br>




Stops delete_type/2 after Limit deleted chars. If Limit < 0, then
stops after -Limit skipped chars.<a name="explode-2"></a>

###explode/2##




<pre>explode(Delimeter::[nonempty_string()], Str::string()) -&gt; [string()]</pre>
<br></br>




Splits the string by delimeters.<a name="explode-3"></a>

###explode/3##




<pre>explode(Delimeter::[nonempty_string()], Str::string(), Limit::integer()) -&gt; string()</pre>
<br></br>


<a name="explode_types-2"></a>

###explode_types/2##




<pre>explode_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string()) -> string()</pre>
<br></br>




Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list.<a name="extract_words-1"></a>

###extract_words/1##




`extract_words(S) -> any()`

<a name="filter_types-2"></a>

###filter_types/2##




<pre>filter_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string()) -> string() | none()</pre>
<br></br>




Returns a new string which is made from the chars of Str
which are a type from Types list.<a name="filter_types-3"></a>

###filter_types/3##




<pre>filter_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string(), Limit::integer()) -> string() | none()</pre>
<br></br>




Stops after -Limit skipped chars.<a name="first-2"></a>

###first/2##




`first(Str, Len) -> any()`



Return Len chars from the beginning of the string.<a name="first_types-3"></a>

###first_types/3##




<pre>first_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string(), Len::integer()) -> string() | none()</pre>
<br></br>




If Len<0, then gets first Len chars of type, which is in Types
If Len>0, then gets first -Len chars of type, which is NOT in Types<a name="freq-1"></a>

###freq/1##




<pre>freq(Str::string()) -&gt; dict()</pre>
<br></br>




Counts how many identical chars in the string.
Returns a dict.
Example:
<pre>  >dict:to_list(ux_string:freq("FFDF")).
  [{70,3},{68,1}]</pre>
<a name="is_nfc-1"></a>

###is_nfc/1##




<pre>is_nfc(Str::list()) -&gt; yes | no | maybe</pre>
<br></br>


<a name="is_nfd-1"></a>

###is_nfd/1##




<pre>is_nfd(Str::list()) -&gt; yes | no | maybe</pre>
<br></br>


<a name="is_nfkc-1"></a>

###is_nfkc/1##




<pre>is_nfkc(Str::list()) -&gt; yes | no | maybe</pre>
<br></br>


<a name="is_nfkd-1"></a>

###is_nfkd/1##




<pre>is_nfkd(Str::list()) -&gt; yes | no | maybe</pre>
<br></br>


<a name="last-2"></a>

###last/2##




`last(Str, Len) -> any()`



Return Len chars from the beginning of the string.<a name="last_types-3"></a>

###last_types/3##




<pre>last_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string(), Len::integer()) -> string() | none()</pre>
<br></br>




If Len<0, then gets last Len chars of type, which is in Types
If Len>0, then gets last -Len chars of type, which is NOT in Types<a name="length-1"></a>

###length/1##




`length(S) -> any()`



Compute count of graphemes in the string.<a name="reverse-1"></a>

###reverse/1##




`reverse(Str) -> any()`



Reverses the string graphemes.<a name="script-1"></a>

###script/1##




`script(S) -> any()`

<a name="scripts-1"></a>

###scripts/1##




`scripts(S) -> any()`

<a name="split-2"></a>

###split/2##




`split(P1, P2) -> any()`

<a name="split-3"></a>

###split/3##




`split(P1, P2, P3) -> any()`

<a name="split_types-2"></a>

###split_types/2##




<pre>split_types(Types::[<a href="#type-char_type">char_type()</a>], Str::string()) -> string()</pre>
<br></br>




Returns a new list of strings which are parts of Str splited
by separator chars of a type from Types list. Parts can not be
empty.<a name="to_graphemes-1"></a>

###to_graphemes/1##




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

###to_lower/1##




<pre>to_lower(Str::string()) -&gt; string()</pre>
<br></br>




Converts characters of a string to a lowercase format.<a name="to_ncr-1"></a>

###to_ncr/1##




`to_ncr(Str) -> any()`



Convert everything from utf-8 into an NCR (Numeric Character Reference).<a name="to_nfc-1"></a>

###to_nfc/1##




<pre>to_nfc(Str::list()) -&gt; list()</pre>
<br></br>


<a name="to_nfd-1"></a>

###to_nfd/1##




<pre>to_nfd(Str::list()) -&gt; list()</pre>
<br></br>


<a name="to_nfkc-1"></a>

###to_nfkc/1##




<pre>to_nfkc(Str::list()) -&gt; list()</pre>
<br></br>


<a name="to_nfkd-1"></a>

###to_nfkd/1##




<pre>to_nfkd(Str::list()) -&gt; list()</pre>
<br></br>


<a name="to_string-1"></a>

###to_string/1##




<pre>to_string(Str::string() | atom() | integer()) -&gt; string()</pre>
<br></br>




Converts something to string (list).<a name="to_upper-1"></a>

###to_upper/1##




<pre>to_upper(Str::string()) -&gt; string()</pre>
<br></br>




Converts characters of a string to a uppercase format.<a name="types-1"></a>

###types/1##




`types(Str) -> any()`



Returns various "character types" which can be used
as a default categorization in implementations.
Types:
http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#General%20Category