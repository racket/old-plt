/*								-*- C -*-
 * File:		parser.y
 * Description: 	Parser for PROLOGIO; can be used with
 *		 either bison or yacc
 */
%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif
    int  yylex(void);
    int  yylook(void);
    int  yywrap(void);
    int  yyback(int *, int);
    void yyerror(char *s);

    void  add_expr(char *);
    char *make_exp(char *str1, char *str2);
    char *make_exp2(char *str1, char *str2, char *str3);
    char *make_integer(char *str);
    char *make_real(char *str1, char *str2);
    char *make_string(char *str);
    char *make_word(char *str);
    char *proio_cons(char * ccar, char * ccdr);
    void  process_command(char * cexpr);
    void  syntax_error(char *s);
#ifdef __cplusplus
}
#endif

%}

%union {
    char *s;
}

%start commands

%token <s> INTEGER 1
%token <s> WORD 2
%token <s> STRING 3
%token <s> PERIOD 13
%token OPEN 4
%token CLOSE 5
%token COMMA 6
%token NEWLINE 7
%token ERROR 8
%token OPEN_SQUARE 9
%token CLOSE_SQUARE 10
%token EQUALS 11
%token EXP 14

%type <s> command expr arglist arg arg1

%%

commands	: /* empty */
		| commands command
		;
command		: WORD PERIOD
		  { process_command(proio_cons(make_word($1), NULL)); free($1); }
		| expr PERIOD
		  { process_command($1); }
		| error PERIOD
		  { syntax_error("Unrecognized command."); }
		;
expr		: WORD OPEN arglist CLOSE 
		  { $$ = proio_cons(make_word($1), $3); free($1); }
		| OPEN_SQUARE CLOSE_SQUARE
                  { $$ = proio_cons(NULL, NULL); }
		| OPEN_SQUARE arglist CLOSE_SQUARE
		  { $$ = $2; }
		;
arglist		:
		  { $$ = NULL; }
		| arg
		  { $$ = proio_cons($1, NULL); }
		| arg COMMA arglist
		  { $$ = proio_cons($1, $3); }
		;
arg		: WORD EQUALS arg1
		  {
		      $$ = proio_cons(make_word("="),
				      proio_cons(make_word($1), proio_cons($3, NULL)));
		      free($1);
		  }
		| arg1
		  { $$ = $1; }
		;
arg1		: WORD
		  {$$ = make_word($1); free($1);}
		| STRING
		  {$$ = make_string($1); free($1);}
		| INTEGER
		  {$$ = make_integer($1); free($1);}
		| INTEGER PERIOD INTEGER
		  {$$ = make_real($1, $3); free($1); free($3); }
	        | INTEGER EXP INTEGER
                  {$$ = make_exp($1, $3); free($1); free($3); }
	        | INTEGER PERIOD INTEGER EXP INTEGER
                  {$$ = make_exp2($1, $3, $5); free($1); free($3); free($5); }
		| expr
		  {$$ = $1;}
		;

%%

#ifdef wx_xt
#	include <./PrologIO/lexer.c>
#else
#	include <lexer.c>
#endif

void yyerror(char *s)
{
    syntax_error(s);
}

int yywrap(void)
{
    return 1;
}
