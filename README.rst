; -- Mode: Markdown; -- ; vim: filetype=markdown tw=76 expandtab
shiftwidth=4 tabstop=4

Unicode eXtension
=================

**License**: `Apache License, Version
2.0 <http://www.apache.org/licenses/LICENSE-2.0.html>`_

**Alternative license**:
`LGPLv3 <http://http://www.gnu.org/licenses/lgpl-3.0.html>`_

**Author**: Uvarov Michael (arcusfelis@gmail.com) 

**Unidata version**: 6.1.0

`Read edoc documentation <https://github.com/erlang-unicode/ux/blob/master/doc/README.md>`_

Module for working with strings. A string is a flatten list of Unicode
characters.

All actions with Unicode were described in the `Unicode
Standards <http://www.unicode.org/reports/>`_.

.. image:: https://secure.travis-ci.org/erlang-unicode/ux.png?branch=master
    :alt: Build Status
    :target: http://travis-ci.org/erlang-unicode/ux


This library realized only these documents:
-------------------------------------------

-  `UAX 15 <http://www.unicode.org/reports/tr15/>`_ Unicode
   Normalization Forms
-  `UTS 10 <http://www.unicode.org/reports/tr10/>`_ Unicode Collation
   Algorithm

and some parts from:
--------------------

-  `UAX 44 <http://www.unicode.org/reports/tr44/>`_ Unicode Character
   Database

Structure of the library
========================

``ux_string`` uses ``ux_char`` and ``ux_unidata``.

``ux_uca`` uses ``ux_char`` and ``ux_unidata``.

``ux_char`` uses ``ux_unidata``.

``ux_unidata`` is for an internal data access.

ux_string.erl: String Functions for lists of Unicode characters.
================================================================

This module provides the functions for operations with
`UNIDATA <http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html>`_.
UNIDATA contains data about Unicode characters.

Functions for working with Unicode Normal Forms (UNF)
-----------------------------------------------------

-  to_nfc/1
-  to_nfd/1
-  to_nfkd/1
-  to_nfkc/1
-  is_nfc/1
-  is_nfd/1
-  is_nfkc/1
-  is_nfkd/1

Functions from stdlib for Unicode strings
-----------------------------------------

-  to_lower/1
-  to_upper/1

Functions for processing strings as groups of graphemes
-------------------------------------------------------

Grapheme is a letter with its modifiers. 
-  length/1 
-  reverse/1
-  first/2
-  last/2

Examples
--------

Code:

.. code-block:: erlang

    (ux@delta)11> ux_string:length("FF g̈").
    4
    (ux@delta)12> string:len("FF g̈").       
    5
    (ux@delta)13> ux_string:to_graphemes("FF g̈").
    ["F","F"," ",[103,776]]

"PHP-style" string functions
----------------------------

-  explode/2,3
-  html_special_chars/1 (htmlspecialchars in php)
-  strip_tags/1,2

Examples
~~~~~~~~

Code:

.. code-block:: erlang

    ux_string:explode(["==", "++", "|"], "+++-+=|==|==|=+-+++").

Result:

.. code-block:: erlang

    [[],"+-+=",[],[],[],[],"=+-","+"]

Code:

.. code-block:: erlang

    ux_html:strip_tags("<b>bold text</b>").

Result:

.. code-block:: erlang

    "bold text"

Types function
--------------

Type is a General Category in Unicode.

Code:

.. code-block:: erlang

    Str = "Erlang created the field of telephone
    networks analysis. His early work in scrutinizing the use of local, exchange
    and trunk telephone line usage in a small community, to understand the
    theoretical requirements of an efficient network led to the creation of the
    Erlang formula, which became a foundational element of present day
    telecommunication network studies.",
    ux_string:explode_types(['Zs', 'Lu'], Str).

Result:

.. code-block:: erlang

    [[],"rlang","created","the","field","of","telephone",
     "networks","analysis.",[],"is","early","work","in",
     "scrutinizing","the","use","of","local,","exchange","and",
     "trunk","telephone","line","usage","in","a","small",
     [...]|...]

Code:

.. code-block:: erlang

    ux_string:types(Str).

Result:

.. code-block:: erlang

    ['Lu','Ll','Ll','Ll','Ll','Ll','Zs','Ll','Ll','Ll','Ll',
     'Ll','Ll','Ll','Zs','Ll','Ll','Ll','Zs','Ll','Ll','Ll','Ll',
     'Ll','Zs','Ll','Ll','Zs','Ll'...]

Where atom ``'Lu'`` is Letter, Uppercase; ll is Letter, Lowercase. Read
more about types from description of ``ux_char:type/1``.

Code:

.. code-block:: erlang

    ux_string:delete_types(['Ll'], Str).

Result:

.. code-block:: erlang

    "E       . H        ,          ,                E ,           ."

ux_char.erl: Char Functions
===========================

Code:

.. code-block:: erlang

    ux_char:type($ ).

Result:

.. code-block:: erlang

    'Zs'

`List of types <http://www.ksu.ru/eng/departments/ktk/test/perl/lib/unicode/UCDFF301.html#General%20Category>`_
---------------------------------------------------------------------------------------------------------------

-  Normative Categories:

   -  Lu Letter, Uppercase
   -  Ll Letter, Lowercase
   -  Lt Letter, Titlecase
   -  Mn Mark, Non-Spacing
   -  Mc Mark, Spacing Combining
   -  Me Mark, Enclosing
   -  Nd Number, Decimal Digit
   -  Nl Number, Letter
   -  No Number, Other
   -  Zs Separator, Space
   -  Zl Separator, Line
   -  Zp Separator, Paragraph
   -  Cc Other, Control
   -  Cf Other, Format
   -  Cs Other, Surrogate
   -  Co Other, Private Use
   -  Cn Other, Not Assigned (no characters in the file have this
      property)

-  Informative Categories:

   -  Lm Letter, Modifier
   -  Lo Letter, Other
   -  Pc Punctuation, Connector
   -  Pd Punctuation, Dash
   -  Ps Punctuation, Open
   -  Pe Punctuation, Close
   -  Pi Punctuation, Initial quote (may behave like Ps or Pe depending
      on usage)
   -  Pf Punctuation, Final quote (may behave like Ps or Pe depending on
      usage)
   -  Po Punctuation, Other
   -  Sm Symbol, Math
   -  Sc Symbol, Currency
   -  Sk Symbol, Modifier
   -  So Symbol, Other

ux_uca.erl: Unicode Collation Algorithm
=======================================

See `Unicode Technical Standard #10 <http://unicode.org/reports/tr10/>`_.

Functions
---------

-  compare/2,3
-  sort/1,2
-  sort_key/1,2
-  sort_array/1,2
-  search/2,3,4

Examples
--------

Code from erlang shell:

.. code-block:: erlang

    1> ux_uca:sort_key("a").   
    <<21,163,0,0,32,0,0,2,0,0,255,255>>

    2> ux_uca:sort_key("abc"). 
    <<21,163,21,185,21,209,0,0,34,0,0,4,0,0,255,255,255,255,
      255,255>>

    3> ux_uca:sort_key("abcd").
    <<21,163,21,185,21,209,21,228,0,0,35,0,0,5,0,0,255,255,
      255,255,255,255,255,255>>

Code:

.. code-block:: erlang

    ux_uca:compare("a", "a").
    ux_uca:compare("a", "b").
    ux_uca:compare("c", "b").

Result:

::

    equal
    lower
    greater

Code:

.. code-block:: erlang

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

Result:

::

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

Searching
---------

Code:

.. code-block:: erlang

    (ux@delta)30> ux_uca:search("The quick brown fox jumps over the lazy dog.",
    "fox").
    {"The quick brown ","fox"," jumps over the lazy dog."}

    (ux@delta)33> ux_uca:search("The quick brown fox jumps over the lazy dog.",
    "cat").         
    false

Searching and Strength
----------------------

Code:

.. code-block:: erlang

    (ux@delta)20> CF = fun(S) -> ux_uca_options:get_options([{strength,S}]) end.      
    #Fun<erl_eval.6.80247286>

    (ux@delta)32> ux_uca:search(CF(2), "The quick brown fox jumps over the lazy
    dog.", "dog", maximal).
    {"The quick brown fox jumps over the lazy"," dog.",[]}

    (ux@delta)21> ux_uca:search(CF(2), "fF", "F").                                    
    {[],"f","F"}

    (ux@delta)22> ux_uca:search(CF(3), "fF", "F").
    {"f","F",[]}

Searching and Match-Style
-------------------------

Code:

.. code-block:: erlang

    (ux@delta)20> CF = fun(S) -> ux_uca_options:get_options([{strength,S}]) end.      
    #Fun<erl_eval.6.80247286>

    (ux@delta)27> ux_uca:search(CF(3), "! F   ?S?", "! F !", 'minimal').
    {"! ","F","   ?S?"}

    (ux@delta)28> ux_uca:search(CF(3), "! F   ?S?", "! F !", 'maximal').
    {[],"! F   ?","S?"}

    (ux@delta)29> ux_uca:search(CF(3), "! F   ?S?", "! F !", 'medium'). 
    {[],"! F ","  ?S?"}

ux_unidata.erl
==============

Stores UNIDATA information. For internal using only.

Data loading
============

.. code-block:: erlang

    ux_unidata_filelist:set_source(Level, ParserType, ImportedDataTypes,
    FromFile).

For example:

.. code-block:: erlang

    ux_unidata_filelist:set_source(process, blocks, all, code:priv_dir(ux) ++ "/UNIDATA/Blocks.txt").

loads data about Unicode blocks from ``priv/UNIDATA/Blocks.txt``.

So, different processes can use their own unidata dictionaries.

Level is ``process``, ``application`` or ``node``.

Parsers are located into ``ux_unidata_parser_*`` modules.

Default unidata files are loaded when the application tries get the
access to them.
