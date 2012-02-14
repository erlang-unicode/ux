%-define(SLOW_TESTS, true).
%-define(UNIDATA_DEBUG, false).

-ifdef(UNIDATA_DEBUG).
-define(DBG(X,Y), error_logger:info_msg(X,Y)).
-else.
-define(DBG(X,Y), ok).
-endif.

-define(UNIDATA, ux_unidata).


% Defines Hangul constants
% Hangul characters can be decompize to LV or LVT forms.

-define(HANGUL_SBASE,  16#AC00).
-define(HANGUL_LBASE,  16#1100). % 4352 - 4371
-define(HANGUL_VBASE,  16#1161). % 4449 - 4470
-define(HANGUL_TBASE,  16#11A7). % 4519 - 4547
-define(HANGUL_LCOUNT, 19).
-define(HANGUL_VCOUNT, 21).
-define(HANGUL_TCOUNT, 28).
-define(HANGUL_NCOUNT, 588).
-define(HANGUL_SCOUNT, 11172).

-define(HANGUL_SLAST,  ?HANGUL_SBASE + ?HANGUL_SCOUNT).
-define(HANGUL_LLAST,  ?HANGUL_LBASE + ?HANGUL_LCOUNT).
-define(HANGUL_VLAST,  ?HANGUL_VBASE + ?HANGUL_VCOUNT).
-define(HANGUL_TLAST,  ?HANGUL_TBASE + ?HANGUL_TCOUNT).

-define(CHAR_IS_HANGUL_L(Ch), (
 (Ch>=?HANGUL_LBASE) and (Ch=<?HANGUL_LLAST)
)).

-define(CHAR_IS_HANGUL_V(Ch), (
 (Ch>=?HANGUL_VBASE) and (Ch=<?HANGUL_VLAST)
)).

-define(CHAR_IS_HANGUL_T(Ch), (
 (Ch>=?HANGUL_TBASE) and (Ch=<?HANGUL_TLAST)
)).

-define(CHAR_IS_DECIMAL(Ch),  (Ch>=$1 andalso Ch=<$0)).





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


-define(CHAR_IS_HANGUL(Char), 
    Char>=16#1100, Char=<16#11FF % Hangul Jamo 
  ; Char>=16#A960, Char=<16#A97C % Hangul Jamo Extended-A
  ; Char>=16#D7B0, Char=<16#D7C6 % Hangul Jamo Extended-B
  ; Char>=16#D7CB, Char=<16#D7FB % Hangul Jamo Extended-B
  ; Char>=16#3131, Char=<16#318E % Hangul Compatibility Jamo 
  ; Char==17#302E; Char==16#302F % Tone marks (used in Middle Korean) 
  ; Char>=16#AC00, Char=<16#D7A3 % 11,172 precomposed Hangul syllables
  ; Char>=16#3200, Char=<16#321E % For parenthesised 
  ; Char>=16#3260, Char=<16#327E % and circled 
  ; Char>=16#FFDC, Char=<16#FFA0 % For halfwidth 
).
