/*								-*- C -*-
 * File:		lexer.l
 * Description: 	Lexical analyser for PROLOGIO; can be used with
 *		either lex and flex.
 */
%{

#if defined(FLEX_SCANNER)
#define PROIO_input my_input
#endif
#if !defined(FLEX_SCANNER)
static int my_unput(char);
#endif
static int my_input(void);

static size_t      lex_buffer_length = 0;
static const char *lex_buffer = NULL;
static size_t      lex_string_ptr = 0;
static int         lex_read_from_string = 0;

#ifdef FLEX_SCANNER
#   undef  YY_INPUT
#   define YY_INPUT(buf,result,max_size)						\
	if (lex_read_from_string) {							\
	    int c = my_input();								\
	    result = (c == 0) ? YY_NULL : ((buf)[0]=(c), 1);				\
	} else if ( (result = read( fileno(yyin), (char *) buf, max_size )) < 0 )	\
	    YY_FATAL_ERROR("read() in flex scanner failed");
#else
#   undef  unput
#   define unput(_c) my_unput(_c)
#endif

%}

SIGN		[+-]
DIGIT		[0-9]
ALPHA		[a-zA-Z_]
ALPHADIGIT	[a-zA-Z_0-9]
STRINGCHAR	[^""\\]
WORDCHAR	[^''\\]

%%

{SIGN}?{DIGIT}+			  {yylval.s = strdup(yytext); return(INTEGER);}

"e"                               return(EXP);

{ALPHA}{ALPHADIGIT}*		  {yylval.s = strdup(yytext); return(WORD);}

"'"{WORDCHAR}*"'"	          {int len = strlen(yytext);
                                   yytext[len-1] = 0;
                                   yylval.s = strdup(yytext+1); 
                                   return(WORD);}

\"({STRINGCHAR}|\\\"|\|\\\\|\\)*\"  {yylval.s = strdup(yytext); return(STRING);}

"("				  return(OPEN);

")"				  return(CLOSE);

","				  return(COMMA);

"["                               return(OPEN_SQUARE);

"]"                               return(CLOSE_SQUARE);

"="                               return(EQUALS);

"."				  return(PERIOD);

[ \t]				  ;

\n				  ;

"/*"                              { loop:
#ifdef __cplusplus
				    while (yyinput() != '*');
				    switch (yyinput())
#else
				    while (input() != '*');
				    switch (input())
#endif
				    {
				        case '/': break;
					case '*': unput('*');
					default: goto loop;
				    }
				  }

.				  return(ERROR);

%%

#ifdef FLEX_SCANNER

static int lex_input()
{
    return input();
}

#else /* !FLEX_SCANNER */

#ifndef input
#   error "Sorry, but need either flex or AT&T lex"
#endif

static int lex_input()
{
    return input();
}

# undef  input
# define input() my_input()

static int my_unput(char c)
{
    if (lex_read_from_string) {
	/* Make sure we have something */
	if (lex_string_ptr) {
	    if (c == '\n') yylineno--;
	    lex_string_ptr--;
	}
    } else {
	yytchar = c;
	if (yytchar == '\n')
	    --yylineno;
	*yysptr++ = yytchar;
    }
    return c;
}

#endif /* !FLEX_SCANNER */

/* Public */ 
void PROIO_LexFromFile(FILE *fd)
{
    lex_read_from_string = 0;
    yyin = fd;
    /* Don't know why this is necessary, but otherwise */
    /* lex only works _once_! */
#ifdef FLEX_SCANNER
    yy_init = 1;
#endif /* FLEX_SCANNER */
}

void PROIO_LexFromString(char *buffer)
{
    lex_read_from_string = 1;
    lex_buffer = buffer;
    lex_buffer_length = strlen(buffer);
    lex_string_ptr = 0;
    /* Don't know why this is necessary, but otherwise */
    /* lex only works _once_! */
#ifdef FLEX_SCANNER
    yy_init = 1;
#endif /* FLEX_SCANNER */
}

static int my_input( void )
{
    if (lex_read_from_string) {
	if (lex_string_ptr == lex_buffer_length)
	    return 0;
	else {
	    char c = lex_buffer[lex_string_ptr++];
#ifndef FLEX_SCANNER
	    if (c == '\n')
		yylineno++;
#endif /* FLEX_SCANNER */
	    return c;
	}
    } else {
	return lex_input();
    }
}
