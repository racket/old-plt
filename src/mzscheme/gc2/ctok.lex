D                       [0-9]
L                       [a-zA-Z_]
H                       [a-fA-F0-9]
E                       [Ee][+-]?{D}+
FS                      (f|F|l|L)
IS                      (u|U|l|L)*

%{
#include <stdio.h>

void count();
%}

%option yylineno

%%
"/*"                    { comment(); }
"//"                    { line_comment(); }
^" "*"#"                { cpp(); }

{L}({L}|{D})*           { count(); symbol(); }

0[xX]{H}+{IS}?          { count(); number(); }
0{D}+{IS}?              { count(); number(); }
{D}+{IS}?               { count(); number(); }
L?'(\\.|[^\\'])+'       { count(); character(); }

{D}+{E}{FS}?            { count(); number(); }
{D}*"."{D}+({E})?{FS}?  { count(); number(); }
{D}+"."{D}*({E})?{FS}?  { count(); number(); }

L?\"(\\.|[^\\"])*\"     { count(); string(); }

"#"                     { count(); h_symbol(); }
"##"                    { count(); hh_symbol(); }
"..."                   { count(); symbol(); }
">>="                   { count(); symbol(); }
"<<="                   { count(); symbol(); }
"+="                    { count(); symbol(); }
"-="                    { count(); symbol(); }
"*="                    { count(); symbol(); }
"/="                    { count(); symbol(); }
"%="                    { count(); symbol(); }
"&="                    { count(); symbol(); }
"^="                    { count(); symbol(); }
"|="                    { count(); boreq_symbol(); }
">>"                    { count(); symbol(); }
"<<"                    { count(); symbol(); }
"++"                    { count(); symbol(); }
"--"                    { count(); symbol(); }
"->"                    { count(); symbol(); }
"&&"                    { count(); symbol(); }
"||"                    { count(); or_symbol(); }
"<="                    { count(); symbol(); }
">="                    { count(); symbol(); }
"=="                    { count(); symbol(); }
"!="                    { count(); symbol(); }
";"                     { count(); xsymbol(); }
("{"|"<%")              { count(); start(); }
("}"|"%>")              { count(); end('{'); }
","                     { count(); xsymbol(); }
":"                     { count(); symbol(); }
"="                     { count(); symbol(); }
"("                     { count(); start(); }
")"                     { count(); end('('); }
("["|"<:")              { count(); start(); }
("]"|":>")              { count(); end('['); }
"."                     { count(); xsymbol(); }
"&"                     { count(); symbol(); }
"!"                     { count(); symbol(); }
"~"                     { count(); symbol(); }
"-"                     { count(); symbol(); }
"+"                     { count(); symbol(); }
"*"                     { count(); symbol(); }
"/"                     { count(); symbol(); }
"%"                     { count(); symbol(); }
"<"                     { count(); symbol(); }
">"                     { count(); symbol(); }
"^"                     { count(); symbol(); }
"|"                     { count(); bor_symbol(); }
"?"                     { count(); symbol(); }

[ \t\v\n\f]             { count(); }
.                       { count(); error(); }

%%

yywrap()
{
        return(1);
}


comment()
{
        char c, c1;

loop:
        while ((c = input()) != '*' && c != 0)
	  ;

        if ((c1 = input()) != '/' && c != 0)
        {
	  unput(c1);
	  goto loop;
        }
}

line_comment()
{
  char c;
  
  while ((c = input()) != '\n' && c != 0)
    ;
}

cpp()
{
  char c, prev;
  
 loop:
  while ((c = input()) != '\n' && c != 0)
    prev = c;

  if (c && prev == '\\')
    goto loop;
}

int start_col = 0;
int start_line = 0;
int old_line = 0;
int column = 0;

void count()
{
        int i;

	start_col = column;
	start_line = old_line;
	old_line = yylineno;

        for (i = 0; yytext[i] != '\0'; i++)
                if (yytext[i] == '\n')
                        column = 0;
                else if (yytext[i] == '\t')
                        column += 8 - (column % 8);
                else
                        column++;
}

error()
{
  fprintf(stderr, "[%d, %d] Unknown character in input: %s\n", yylineno, column, yytext);
  exit(-1);
}

symbol()
{
  printf("(%s %d %d)\n", yytext, start_line, start_col);
}

xsymbol()
{
  printf("(\\%s %d %d)\n", yytext, start_line, start_col);
}

or_symbol()
{
  printf("(\\|\\| %d %d)\n", start_line, start_col);
}

bor_symbol()
{
  printf("(\\| %d %d)\n", start_line, start_col);
}

boreq_symbol()
{
  printf("(\\|= %d %d)\n", start_line, start_col);
}

hh_symbol()
{
  printf("(\\#\\# %d %d)\n", start_line, start_col);
}

h_symbol()
{
  printf("(\\# %d %d)\n", start_line, start_col);
}

number()
{
  return symbol();
}

character()
{
  int i;
  char *s;

  s = yytext;

  printf("(");

 again:

  for (i = 0; s[i]; i++)
    if (s[i] == '|')
      break;
  
  if (s[i]) {
    s[i] = 0;
    printf("|%s|\\|", s);
    s = s + i + 1;
    goto again;
  } else
    printf("|%s|", s);

  printf(" %d %d)\n", start_line, start_col);
}

string()
{
  printf("(%s %d %d)\n", yytext, start_line, start_col);
}

start()
{
  printf("((\"%s\") %d %d\n", yytext, start_line, start_col);
}

end(int c)
{
  printf(")\n");
}

int main()
{
  printf("( ; start\n");
  yylex();
  printf(") ; end\n");
  return 0;
}
