-module(ux_types).

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

-export_type([char_type/0, 
        ux_ccc/0]).
