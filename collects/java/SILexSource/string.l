; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

digit [0123456789]

%%

"\""          (make-tok doublequote-tok yytext yyline yycolumn)
"\\n"         (parse-spec-char          yytext yyline yycolumn)
"\\"{digit}+  (parse-digits-char        yytext yyline yycolumn)
"\\-"{digit}+ (parse-digits-char        yytext yyline yycolumn)
"\\"[^]       (parse-quoted-char        yytext yyline yycolumn)
[^]           (parse-ordinary-char      yytext yyline yycolumn)
<<EOF>>       (make-tok eof-tok         yytext yyline yycolumn)
