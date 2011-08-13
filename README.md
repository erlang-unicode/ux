; -- Mode: Markdown; -- ; vim: filetype=markdown tw=76 expandtab shiftwidth=4 tabstop=4


Unicode eXtension
=================

__License__: [LGPLv3](http://http://www.gnu.org/licenses/lgpl-3.0.html)

__Author__: Uvarov Michael ([`freeakk@gmail.com`](mailto:freeakk@gmail.com))

[Read edoc documentation](https://github.com/freeakk/ux/blob/master/doc/README.md)



All actions with Unicode was described in [Unicode Standards](http://www.unicode.org/reports/).
This library realized only these documents:
------------------------------------------
* [UAX 15](http://www.unicode.org/reports/tr15/)  Unicode Normalization Forms
* [UTS 10](http://www.unicode.org/reports/tr10/)  Unicode Collation Algorithm

and some parts from:
--------------------
* [UAX 44](http://www.unicode.org/reports/tr44/) Unicode Character Database


Structure of the library
========================
`ux_string` uses `ux_char` and `ux_unidata`.

`ux_uca` uses `ux_char` and `ux_unidata`.

`ux_char` uses `ux_unidata`.

`ux_unidata` is for an internal data access.


ux\_string.erl: String Functions for lists of Unicode characters.
================================================================

This module provides the functions for operations with
[UNIDATA](http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html).
UNIDATA contains data about Unicode characters.


Functions for working with Unicode Normal Forms (UNF)
-----------------------------------------------------
* to\_nfc/1
* to\_nfd/1
* to\_nfkd/1
* to\_nfkc/1
* is\_nfc/1
* is\_nfd/1
* is\_nfkc/1
* is\_nfkd/1

Functions from stdlib for Unicode strings
-----------------------------------------
* to\_lower/1
* to\_upper/1

Functions for processing strings as groups of graphemes
-------------------------------------------------------
Grapheme is a letter with its modifiers.
* length/1
* reverse/1
* first/2
* last/2

"PHP-style" string functions
----------------------------
* explode/2,3
* html\_special\_chars/1 (htmlspecialchars in php)
* strip\_tags/1,2

Examples
--------
Code:

```erlang
ux_string:explode(["==", "++", "|"], "+++-+=|==|==|=+-+++").
```

Result:

```
[[],"+-+=",[],[],[],[],"=+-","+"]
```

Code:

```erlang
ux_string:strip_tags("<b>bold text</b>").
```

Result:

```
"bold text"
```

Code:

```erlang
Str = "Erlang created the field of telephone
networks analysis. His early work in scrutinizing the use of local, exchange
and trunk telephone line usage in a small community, to understand the
theoretical requirements of an efficient network led to the creation of the
Erlang formula, which became a foundational element of present day
telecommunication network studies."
ux_string:explode_types([zs, lu], Str).
```

Result:

```
[[],"rlang","created","the","field","of","telephone",
 "networks","analysis.",[],"is","early","work","in",
 "scrutinizing","the","use","of","local,","exchange","and",
 "trunk","telephone","line","usage","in","a","small",
 [...]|...]
```

Code:

```erlang
ux_string:types(Str).
```

Result: 

```
[lu,ll,ll,ll,ll,ll,zs,ll,ll,ll,ll,ll,ll,ll,zs,ll,ll,ll,zs,
 ll,ll,ll,ll,ll,zs,ll,ll,zs,ll|...]
```
Where ``lu`` is Letter, Uppercase; ll is Letter, Lowercase. Read more about
types from description of ``ux_char:type/1``.

Code:

```erlang
ux_string:delete_types([ll], Str).
```

Result:

```
"E       . H        ,          ,                E ,           ."
```

ux\_char.erl: Char Functions
============================
Code:

```erlang
ux_char:type($ ).
```

Result:

```
zs
```
[List of types](http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#General%20Category)
---------------
* Normative Categories:
    * lu  Letter, Uppercase
    * ll  Letter, Lowercase
    * lt  Letter, Titlecase
    * mn  Mark, Non-Spacing
    * mc  Mark, Spacing Combining
    * me  Mark, Enclosing
    * nd  Number, Decimal Digit
    * nl  Number, Letter
    * no  Number, Other
    * zs  Separator, Space
    * zl  Separator, Line
    * zp  Separator, Paragraph
    * cc  Other, Control
    * cf  Other, Format
    * cs  Other, Surrogate
    * co  Other, Private Use
    * cn  Other, Not Assigned (no characters in the file have this property)
* Informative Categories:
    * lm  Letter, Modifier
    * lo  Letter, Other
    * pc  Punctuation, Connector
    * pd  Punctuation, Dash
    * ps  Punctuation, Open
    * pe  Punctuation, Close
    * pi  Punctuation, Initial quote (may behave like Ps or Pe depending on usage)
    * pf  Punctuation, Final quote (may behave like Ps or Pe depending on usage)
    * po  Punctuation, Other
    * sm  Symbol, Math
    * sc  Symbol, Currency
    * sk  Symbol, Modifier
    * so  Symbol, Other

ux\_uca.erl: Unicode Collation Algorithm
========================================
See [Unicode Technical Standard #10](http://unicode.org/reports/tr10/).

Functions
---------
* compare/2,3
* sort/1,2
* sort_key/1,2
* sort_array/1,2

Examples
--------
Code from erlang shell:
```erlang
1> ux_uca:sort_key("a").   
<<21,163,0,0,32,0,0,2,0,0,255,255>>

2> ux_uca:sort_key("abc"). 
<<21,163,21,185,21,209,0,0,34,0,0,4,0,0,255,255,255,255,
  255,255>>

3> ux_uca:sort_key("abcd").
<<21,163,21,185,21,209,21,228,0,0,35,0,0,5,0,0,255,255,
  255,255,255,255,255,255>>
```

Code:

```erlang
ux_uca:compare("a", "a").
ux_uca:compare("a", "b").
ux_uca:compare("c", "b").
```

Result:

```
equal
lower
greater
```

Code: 

```erlang
Options = ux_uca_options:get_options([ 
        {natural_sort, false}, 
        {strength, 3}, 
        {alternate, shifted} 
    ]),
InStrings = ["erlang", "esl", "nitrogen", "epm", "mochiweb", "rebar", "eunit"],
OutStrings = ux_uca:sort(Options, InStrings),
[io:format("~ts~n", [S]) || S <- OutStrings],

SortKeys = [{Str, ux_uca:sort_key(Options, Str)} || Str <- OutStrings],
[io:format("~ts ~w~n", [S, K]) || {S, K} <- SortKeys],

ok.
```

Result:

```
epm
erlang
esl
eunit
mochiweb
nitrogen
rebar
epm [5631,5961,5876,0,32,32,32,0,2,2,2]
erlang [5631,6000,5828,5539,5890,5700,0,32,32,32,32,32,32,0,2,2,2,2,2,2]
esl [5631,6054,5828,0,32,32,32,0,2,2,2]
eunit [5631,6121,5890,5760,6089,0,32,32,32,32,32,0,2,2,2,2,2]
mochiweb [5876,5924,5585,5735,5760,6180,5631,5561,0,32,32,32,32,32,32,32,32,0,2,2,2,2,2,2,2,2]
nitrogen [5890,5760,6089,6000,5924,5700,5631,5890,0,32,32,32,32,32,32,32,32,0,2,2,2,2,2,2,2,2]
rebar [6000,5631,5561,5539,6000,0,32,32,32,32,32,0,2,2,2,2,2]
ok
```


ux\_unidata.erl
===============
Stores UNIDATA information. For internal using only.

Data loading
============

```erlang 
ux_unidata_filelist:set_source(Level, ParserType, ImportedDataTypes,
FromFile).
```

For example:

```erlang
ux_unidata_filelist:set_source(process, blocks, all, code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt"}).
```

loads data about Unicode blocks from `priv/UNIDATA/Blocks.txt`.

So, different processes can use their own unidata dictionaries.

Level is `process`, `application` or `node`.

Parsers are located into ``ux_unidata_parser_*`` modules.

Default unidata files are loaded when the application tries get the access to
them.
