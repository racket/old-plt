; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

space   " "
tab     "	"
comment ";".*
hblank  {space}|{tab}|{comment}
vblank  "\n"

digit   [0123456789]
letter  [abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ]
specini "!"|"$"|"%"|"&"|"*"|"/"|":"|"<"|"="|">"|"?"|"~"|"_"|"^"
specsub "."|"+"|"-"
initial {letter}|{specini}
subseq  {letter}|{digit}|{specini}|{specsub}
peculid "+"|"-"|"..."
id      {initial}{subseq}*|{peculid}

%%

{hblank}                  (make-tok hblank-tok        yytext yyline yycolumn)
{vblank}                  (make-tok vblank-tok        yytext yyline yycolumn)
"|"                       (make-tok pipe-tok          yytext yyline yycolumn)
"?"                       (make-tok question-tok      yytext yyline yycolumn)
"+"                       (make-tok plus-tok          yytext yyline yycolumn)
"*"                       (make-tok star-tok          yytext yyline yycolumn)
"("                       (make-tok lpar-tok          yytext yyline yycolumn)
")"                       (make-tok rpar-tok          yytext yyline yycolumn)
"."                       (make-tok dot-tok           yytext yyline yycolumn)
"["                       (make-tok lbrack-tok        yytext yyline yycolumn)
"[]"                      (make-tok lbrack-rbrack-tok yytext yyline yycolumn)
"[^"                      (make-tok lbrack-caret-tok  yytext yyline yycolumn)
"[-"                      (make-tok lbrack-minus-tok  yytext yyline yycolumn)
"{"{id}"}"                (parse-id-ref               yytext yyline yycolumn)
"{"{digit}+"}"            (parse-power-m              yytext yyline yycolumn)
"{"{digit}+",}"           (parse-power-m-inf          yytext yyline yycolumn)
"{"{digit}+","{digit}+"}" (parse-power-m-n            yytext yyline yycolumn)
"{"                       (make-tok illegal-tok       yytext yyline yycolumn)
"\""                      (make-tok doublequote-tok   yytext yyline yycolumn)
"\\n"                     (parse-spec-char            yytext yyline yycolumn)
"\\"{digit}+              (parse-digits-char          yytext yyline yycolumn)
"\\-"{digit}+             (parse-digits-char          yytext yyline yycolumn)
"\\"[^]                   (parse-quoted-char          yytext yyline yycolumn)
"^"                       (make-tok caret-tok         yytext yyline yycolumn)
"$"                       (make-tok dollar-tok        yytext yyline yycolumn)
.                         (parse-ordinary-char        yytext yyline yycolumn)
"<<EOF>>"                 (make-tok <<EOF>>-tok       yytext yyline yycolumn)
"<<ERROR>>"               (make-tok <<ERROR>>-tok     yytext yyline yycolumn)
<<EOF>>                   (make-tok eof-tok           yytext yyline yycolumn)
