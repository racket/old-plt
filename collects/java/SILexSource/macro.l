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

{hblank} (make-tok hblank-tok          yytext yyline yycolumn)
{vblank} (make-tok vblank-tok          yytext yyline yycolumn)
"%%"     (make-tok percent-percent-tok yytext yyline yycolumn)
{id}     (parse-id                     yytext yyline yycolumn)
[^]      (make-tok illegal-tok         yytext yyline yycolumn)
<<EOF>>  (make-tok eof-tok             yytext yyline yycolumn)
