% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
%%% =====================================================================
%%% This library is free software; you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This library is distributed in the hope that it will be useful, but
%%% WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%%% Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with this library; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%%% USA
%%%
%%% $Id$
%%%
%%% @copyright 2010-2011 Michael Uvarov
%%% @author Michael Uvarov <freeakk@gmail.com>
%%% @see ux
%%% @end
%%% =====================================================================

-type char_type() ::     
% Normative Categories:
      lu % Letter, Uppercase
    | ll % Letter, Lowercase
    | lt % Letter, Titlecase
    | mn % Mark, Non-Spacing
    | mc % Mark, Spacing Combining
    | me % Mark, Enclosing
    | nd % Number, Decimal Digit
    | nl % Number, Letter
    | no % Number, Other
    | zs % Separator, Space
    | zl % Separator, Line
    | zp % Separator, Paragraph
    | cc % Other, Control
    | cf % Other, Format
    | cs % Other, Surrogate
    | co % Other, Private Use
    | cn % Other, Not Assigned (no characters in the file have this property)
% Informative Categories:
    | lm % Letter, Modifier
    | lo % Letter, Other
    | pc % Punctuation, Connector
    | pd % Punctuation, Dash
    | ps % Punctuation, Open
    | pe % Punctuation, Close
    | pi % Punctuation, Initial quote (may behave like Ps or Pe depending on
         % usage)
    | pf % Punctuation, Final quote (may behave like Ps or Pe depending on usage)
    | po % Punctuation, Other
    | sm % Symbol, Math
    | sc % Symbol, Currency
    | sk % Symbol, Modifier
    | so % Symbol, Other
    | other
.

-type ux_ccc() :: 0..240.

% CJK_Unified_Ideograph and CJK_Compatibility_Ideographs from 
% http://www.unicode.org/Public/UNIDATA/Blocks.txt
-define(CHAR_IS_CJK_UNIFIED_IDEOGRAPH(Ch), (
    (Ch >= 16#4E00) and (Ch =< 16#9FFF) % CJK Unified Ideographs
)).
-define(CHAR_IS_CJK_COMPATIBILITY_IDEOGRAPH(Ch), (
    (Ch >= 16#F900) and (Ch =< 16#FAFF) % CJK Compatibility Ideographs
)).

% Unified_Ideograph from http://unicode.org/Public/UNIDATA/PropList.txt
-define(CHAR_IS_UNIFIED_IDEOGRAPH(Ch), (
% [6582] CJK UNIFIED IDEOGRAPH-3400..4DB5
    ((Ch >= 16#3400)  and (Ch =< 16#4DB5))

% [20940] CJK UNIFIED IDEOGRAPH-4E00..9FCB
or ((Ch >= 16#4E00)  and (Ch =< 16#9FCB))
% FIXED: Error: [55296,33] lower [40908,98]
% CJK Unified Ideographs
%or ((Ch >= 16#4E00)  and (Ch =< 16#9FFF)) 

% [2] CJK COMPATIBILITY IDEOGRAPH-FA0E..FA0F
 or ((Ch >= 16#FA0E)  and (Ch =< 16#FA0F))

 or ((Ch == 16#FA11)                     ) % CJK COMPATIBILITY IDEOGRAPH-FA11

% [2] CJK COMPATIBILITY IDEOGRAPH-FA13..FA14
 or ((Ch >= 16#FA13)  and (Ch =< 16#FA14))

 or ((Ch == 16#FA1F)                     ) % CJK COMPATIBILITY IDEOGRAPH-FA1F
 or ((Ch == 16#FA21)                     ) % CJK COMPATIBILITY IDEOGRAPH-FA21

% [2] CJK COMPATIBILITY IDEOGRAPH-FA23..FA24
 or ((Ch >= 16#FA23)  and (Ch =< 16#FA24))

% [3] CJK COMPATIBILITY IDEOGRAPH-FA27..FA29 
 or ((Ch >= 16#FA27)  and (Ch =< 16#FA29))

% [42711] CJK UNIFIED IDEOGRAPH-20000..2A6D6
 or ((Ch >= 16#20000) and (Ch =< 16#2A6D6))

% [4149] CJK UNIFIED IDEOGRAPH-2A700..2B734
 or ((Ch >= 16#2A700) and (Ch =< 16#2B734))

% [222] CJK UNIFIED IDEOGRAPH-2B740..2B81D 
 or ((Ch >= 16#2B740) and (Ch =< 16#2B81D))
)).
