
% Hangul & UCA
-define(COL_HANGUL_LBASE,  12337). % 12337 - 12356
-define(COL_HANGUL_VBASE,  12463). % 12463 - 12484
-define(COL_HANGUL_TBASE,  12533). % 12533 - 12584

-define(COL_HANGUL_LLAST,  ?COL_HANGUL_LBASE + ?HANGUL_LCOUNT).
-define(COL_HANGUL_VLAST,  ?COL_HANGUL_VBASE + ?HANGUL_VCOUNT).
-define(COL_HANGUL_TLAST,  ?COL_HANGUL_TBASE + ?HANGUL_TCOUNT).

% Weight on level 1 (L1) is L1 of Hangul jamo L.
-define(IS_L1_OF_HANGUL_L(Ch), (
 (Ch>=?COL_HANGUL_LBASE) and (Ch=<?COL_HANGUL_LLAST)
)).

% Weight on level 1 (L1) is L1 of Hangul jamo V.
-define(IS_L1_OF_HANGUL_V(Ch), (
 (Ch>=?COL_HANGUL_VBASE) and (Ch=<?COL_HANGUL_VLAST)
)).

% Weight on level 1 (L1) is L1 of Hangul jamo T.
-define(IS_L1_OF_HANGUL_T(Ch), (
 (Ch>=?COL_HANGUL_TBASE) and (Ch=<?COL_HANGUL_TLAST)
)).

