; Copyright (C) 1997 Danny Dube', Universite' de Montre'al.
; All rights reserved.
; SILex 1.0.

space   " "
tab     "	"
comment ";".*
hblank  {space}|{tab}|{comment}
vblank  "\n"

%%

{hblank}+ (make-tok hblank-tok yytext yyline yycolumn)
{vblank}  (make-tok vblank-tok yytext yyline yycolumn)
.+        (make-tok char-tok   yytext yyline yycolumn)
<<EOF>>   (make-tok eof-tok    yytext yyline yycolumn)
