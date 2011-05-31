% vim: set filetype=erlang shiftwidth=4 tabstop=4 expandtab tw=80:
%%% User Extentions for Erlang 
%%%
%%% @package  ux_char
%%% @author   Uvarov Michael <freeakk@gmail.com>
%%% @license  http://www.fsf.org/copyleft/lgpl.html LGPL
%%%
%%% @copyright 2010 Uvarov Michael.
%%% %CopyrightBegin%
%%%  Copyright 2010 Uvarov Michael  
%%%
%%%  See the enclosed file COPYING for license information (LGPL). If you
%%%  did not receive this file, see http://www.fsf.org/copyleft/lgpl.html
%%% %CopyrightEnd%

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
