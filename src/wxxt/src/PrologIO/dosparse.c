
/*  A Bison parser, made from PrologIO/parser.y with Bison version GNU Bison version 1.22
  */

#define YYBISON 1  /* Identify Bison output.  */

#define	INTEGER	1
#define	WORD	2
#define	STRING	3
#define	PERIOD	13
#define	OPEN	4
#define	CLOSE	5
#define	COMMA	6
#define	NEWLINE	7
#define	ERROR	8
#define	OPEN_SQUARE	9
#define	CLOSE_SQUARE	10
#define	EQUALS	11
#define	EXP	14

#line 6 "PrologIO/parser.y"


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#ifdef __cplusplus
extern "C" {
#endif
    int  PROIO_yylex(void);
    int  PROIO_yylook(void);
    int  PROIO_yywrap(void);
    int  PROIO_yyback(int *, int);
    void PROIO_yyerror(char *s);

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


#line 38 "PrologIO/parser.y"
typedef union {
    char *s;
} YYSTYPE;

#ifndef YYLTYPE
typedef
  struct PROIO_yyltype
    {
      int timestamp;
      int first_line;
      int first_column;
      int last_line;
      int last_column;
      char *text;
   }
  PROIO_yyltype;

#define YYLTYPE PROIO_yyltype
#endif

#include <stdio.h>

#ifndef __cplusplus
#ifndef __STDC__
#define const
#endif
#endif



#define	YYFINAL		34
#define	YYFLAG		-32768
#define	YYNTBASE	16

#define YYTRANSLATE(x) ((unsigned)(x) <= 257 ? PROIO_yytranslate[x] : 22)

static const char PROIO_yytranslate[] = {     0,
     3,     4,     5,     7,     8,     9,    10,    11,    12,    13,
    14,     2,     6,    15,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     1,     2
};

#if YYDEBUG != 0
static const short PROIO_yyprhs[] = {     0,
     0,     1,     4,     7,    10,    13,    18,    21,    25,    26,
    28,    32,    36,    38,    40,    42,    44,    48,    52,    58
};

static const short PROIO_yyrhs[] = {    -1,
    16,    17,     0,     4,     6,     0,    18,     6,     0,     1,
     6,     0,     4,     7,    19,     8,     0,    12,    13,     0,
    12,    19,    13,     0,     0,    20,     0,    20,     9,    19,
     0,     4,    14,    21,     0,    21,     0,     4,     0,     5,
     0,     3,     0,     3,     6,     3,     0,     3,    15,     3,
     0,     3,     6,     3,    15,     3,     0,    18,     0
};

#endif

#if YYDEBUG != 0
static const short PROIO_yyrline[] = { 0,
    62,    63,    65,    67,    69,    72,    74,    76,    79,    81,
    83,    86,    92,    95,    97,    99,   101,   103,   105,   107
};

static const char * const PROIO_yytname[] = {   "$","error","$illegal.","INTEGER",
"WORD","STRING","PERIOD","OPEN","CLOSE","COMMA","NEWLINE","ERROR","OPEN_SQUARE",
"CLOSE_SQUARE","EQUALS","EXP","commands","command","expr","arglist","arg","arg1",
""
};
#endif

static const short PROIO_yyr1[] = {     0,
    16,    16,    17,    17,    17,    18,    18,    18,    19,    19,
    19,    20,    20,    21,    21,    21,    21,    21,    21,    21
};

static const short PROIO_yyr2[] = {     0,
     0,     2,     2,     2,     2,     4,     2,     3,     0,     1,
     3,     3,     1,     1,     1,     1,     3,     3,     5,     1
};

static const short PROIO_yydefact[] = {     1,
     0,     0,     0,     0,     2,     0,     5,     3,     9,    16,
    14,    15,     7,    20,     0,    10,    13,     4,     0,     0,
     0,     0,     8,     9,     6,    17,    18,    14,    12,    11,
     0,    19,     0,     0
};

static const short PROIO_yydefgoto[] = {     1,
     5,    14,    15,    16,    17
};

static const short PROIO_yypact[] = {-32768,
     1,     0,    -3,     4,-32768,    18,-32768,-32768,     7,     8,
    11,-32768,-32768,-32768,    13,    19,-32768,-32768,    22,    24,
    28,    17,-32768,     7,-32768,    20,-32768,    25,-32768,-32768,
    30,-32768,    34,-32768
};

static const short PROIO_yypgoto[] = {-32768,
-32768,    35,    -9,-32768,    15
};


#define	YYLAST		37


static const short PROIO_yytable[] = {    19,
    33,     2,     8,     9,     3,     7,    10,    11,    12,    10,
    11,    12,     4,    20,    30,     4,    13,     9,     4,    10,
    28,    12,    21,    18,    22,    23,    26,    24,     4,    25,
    27,     9,    32,    34,    31,     6,    29
};

static const short PROIO_yycheck[] = {     9,
     0,     1,     6,     7,     4,     6,     3,     4,     5,     3,
     4,     5,    12,     6,    24,    12,    13,     7,    12,     3,
     4,     5,    15,     6,    14,    13,     3,     9,    12,     8,
     3,     7,     3,     0,    15,     1,    22
};
/* -*-C-*-  Note some compilers choke on comments on `#line' lines.  */
#line 3 "/usr/lib/bison.simple"

/* Skeleton output parser for bison,
   Copyright (C) 1984, 1989, 1990 Bob Corbett and Richard Stallman

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */


#ifndef alloca
#ifdef __GNUC__
#define alloca __builtin_alloca
#else /* not GNU C.  */
#if (!defined (__STDC__) && defined (sparc)) || defined (__sparc__) || defined (__sparc) || defined (__sgi)
#include <alloca.h>
#else /* not sparc */
#if defined (MSDOS) && !defined (__TURBOC__)
#include <malloc.h>
#else /* not MSDOS, or __TURBOC__ */
#if defined(_AIX)
#include <malloc.h>
 #pragma alloca
#else /* not MSDOS, __TURBOC__, or _AIX */
#ifdef __hpux
#ifdef __cplusplus
extern "C" {
void *alloca (unsigned int);
};
#else /* not __cplusplus */
void *alloca ();
#endif /* not __cplusplus */
#endif /* __hpux */
#endif /* not _AIX */
#endif /* not MSDOS, or __TURBOC__ */
#endif /* not sparc.  */
#endif /* not GNU C.  */
#endif /* alloca not defined.  */

/* This is the parser code that is written into each bison parser
  when the %semantic_parser declaration is not specified in the grammar.
  It was written by Richard Stallman by simplifying the hairy parser
  used when %semantic_parser is specified.  */

/* Note: there must be only one dollar sign in this file.
   It is replaced by the list of actions, each action
   as one case of the switch.  */

#define PROIO_yyerrok		(PROIO_yyerrstatus = 0)
#define PROIO_yyclearin	(PROIO_yychar = YYEMPTY)
#define YYEMPTY		-2
#define YYEOF		0
#define YYACCEPT	return(0)
#define YYABORT 	return(1)
#define YYERROR		goto PROIO_yyerrlab1
/* Like YYERROR except do call PROIO_yyerror.
   This remains here temporarily to ease the
   transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */
#define YYFAIL		goto PROIO_yyerrlab
#define YYRECOVERING()  (!!PROIO_yyerrstatus)
#define YYBACKUP(token, value) \
do								\
  if (PROIO_yychar == YYEMPTY && PROIO_yylen == 1)				\
    { PROIO_yychar = (token), PROIO_yylval = (value);			\
      PROIO_yychar1 = YYTRANSLATE (PROIO_yychar);				\
      YYPOPSTACK;						\
      goto PROIO_yybackup;						\
    }								\
  else								\
    { PROIO_yyerror ("syntax error: cannot back up"); YYERROR; }	\
while (0)

#define YYTERROR	1
#define YYERRCODE	256

#ifndef YYPURE
#define YYLEX		PROIO_yylex()
#endif

#ifdef YYPURE
#ifdef YYLSP_NEEDED
#define YYLEX		PROIO_yylex(&PROIO_yylval, &PROIO_yylloc)
#else
#define YYLEX		PROIO_yylex(&PROIO_yylval)
#endif
#endif

/* If nonreentrant, generate the variables here */

#ifndef YYPURE

int	PROIO_yychar;			/*  the lookahead symbol		*/
YYSTYPE	PROIO_yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#ifdef YYLSP_NEEDED
YYLTYPE PROIO_yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int PROIO_yynerrs;			/*  number of parse errors so far       */
#endif  /* not YYPURE */

#if YYDEBUG != 0
int PROIO_yydebug;			/*  nonzero means print parse trace	*/
/* Since this is uninitialized, it does not stop multiple parsers
   from coexisting.  */
#endif

/*  YYINITDEPTH indicates the initial size of the parser's stacks	*/

#ifndef	YYINITDEPTH
#define YYINITDEPTH 200
#endif

/*  YYMAXDEPTH is the maximum size the stacks can grow to
    (effective only if the built-in stack extension method is used).  */

#if YYMAXDEPTH == 0
#undef YYMAXDEPTH
#endif

#ifndef YYMAXDEPTH
#define YYMAXDEPTH 10000
#endif

/* Prevent warning if -Wstrict-prototypes.  */
#ifdef __GNUC__
int PROIO_yyparse (void);
#endif

#if __GNUC__ > 1		/* GNU C and GNU C++ define this.  */
#define __PROIO_yy_bcopy(FROM,TO,COUNT)	__builtin_memcpy(TO,FROM,COUNT)
#else				/* not GNU C or C++ */
#ifndef __cplusplus

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__PROIO_yy_bcopy (from, to, count)
     char *from;
     char *to;
     int count;
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#else /* __cplusplus */

/* This is the most reliable way to avoid incompatibilities
   in available built-in functions on various systems.  */
static void
__PROIO_yy_bcopy (char *from, char *to, int count)
{
  register char *f = from;
  register char *t = to;
  register int i = count;

  while (i-- > 0)
    *t++ = *f++;
}

#endif
#endif

#line 184 "/usr/lib/bison.simple"
int
PROIO_yyparse()
{
  register int PROIO_yystate;
  register int PROIO_yyn;
  register short *PROIO_yyssp;
  register YYSTYPE *PROIO_yyvsp;
  int PROIO_yyerrstatus;	/*  number of tokens to shift before error messages enabled */
  int PROIO_yychar1 = 0;		/*  lookahead token as an internal (translated) token number */

  short	PROIO_yyssa[YYINITDEPTH];	/*  the state stack			*/
  YYSTYPE PROIO_yyvsa[YYINITDEPTH];	/*  the semantic value stack		*/

  short *PROIO_yyss = PROIO_yyssa;		/*  refer to the stacks thru separate pointers */
  YYSTYPE *PROIO_yyvs = PROIO_yyvsa;	/*  to allow PROIO_yyoverflow to reallocate them elsewhere */

#ifdef YYLSP_NEEDED
  YYLTYPE PROIO_yylsa[YYINITDEPTH];	/*  the location stack			*/
  YYLTYPE *PROIO_yyls = PROIO_yylsa;
  YYLTYPE *PROIO_yylsp;

#define YYPOPSTACK   (PROIO_yyvsp--, PROIO_yyssp--, PROIO_yylsp--)
#else
#define YYPOPSTACK   (PROIO_yyvsp--, PROIO_yyssp--)
#endif

  int PROIO_yystacksize = YYINITDEPTH;

#ifdef YYPURE
  int PROIO_yychar;
  YYSTYPE PROIO_yylval;
  int PROIO_yynerrs;
#ifdef YYLSP_NEEDED
  YYLTYPE PROIO_yylloc;
#endif
#endif

  YYSTYPE PROIO_yyval;		/*  the variable used to return		*/
				/*  semantic values from the action	*/
				/*  routines				*/

  int PROIO_yylen;

#if YYDEBUG != 0
  if (PROIO_yydebug)
    fprintf(stderr, "Starting parse\n");
#endif

  PROIO_yystate = 0;
  PROIO_yyerrstatus = 0;
  PROIO_yynerrs = 0;
  PROIO_yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  PROIO_yyssp = PROIO_yyss - 1;
  PROIO_yyvsp = PROIO_yyvs;
#ifdef YYLSP_NEEDED
  PROIO_yylsp = PROIO_yyls;
#endif

/* Push a new state, which is found in  PROIO_yystate  .  */
/* In all cases, when you get here, the value and location stacks
   have just been pushed. so pushing a state here evens the stacks.  */
PROIO_yynewstate:

  *++PROIO_yyssp = PROIO_yystate;

  if (PROIO_yyssp >= PROIO_yyss + PROIO_yystacksize - 1)
    {
      /* Give user a chance to reallocate the stack */
      /* Use copies of these so that the &'s don't force the real ones into memory. */
      YYSTYPE *PROIO_yyvs1 = PROIO_yyvs;
      short *PROIO_yyss1 = PROIO_yyss;
#ifdef YYLSP_NEEDED
      YYLTYPE *PROIO_yyls1 = PROIO_yyls;
#endif

      /* Get the current used size of the three stacks, in elements.  */
      int size = PROIO_yyssp - PROIO_yyss + 1;

#ifdef PROIO_yyoverflow
      /* Each stack pointer address is followed by the size of
	 the data in use in that stack, in bytes.  */
#ifdef YYLSP_NEEDED
      /* This used to be a conditional around just the two extra args,
	 but that might be undefined if PROIO_yyoverflow is a macro.  */
      PROIO_yyoverflow("parser stack overflow",
		 &PROIO_yyss1, size * sizeof (*PROIO_yyssp),
		 &PROIO_yyvs1, size * sizeof (*PROIO_yyvsp),
		 &PROIO_yyls1, size * sizeof (*PROIO_yylsp),
		 &PROIO_yystacksize);
#else
      PROIO_yyoverflow("parser stack overflow",
		 &PROIO_yyss1, size * sizeof (*PROIO_yyssp),
		 &PROIO_yyvs1, size * sizeof (*PROIO_yyvsp),
		 &PROIO_yystacksize);
#endif

      PROIO_yyss = PROIO_yyss1; PROIO_yyvs = PROIO_yyvs1;
#ifdef YYLSP_NEEDED
      PROIO_yyls = PROIO_yyls1;
#endif
#else /* no PROIO_yyoverflow */
      /* Extend the stack our own way.  */
      if (PROIO_yystacksize >= YYMAXDEPTH)
	{
	  PROIO_yyerror("parser stack overflow");
	  return 2;
	}
      PROIO_yystacksize *= 2;
      if (PROIO_yystacksize > YYMAXDEPTH)
	PROIO_yystacksize = YYMAXDEPTH;
      PROIO_yyss = (short *) alloca (PROIO_yystacksize * sizeof (*PROIO_yyssp));
      __PROIO_yy_bcopy ((char *)PROIO_yyss1, (char *)PROIO_yyss, size * sizeof (*PROIO_yyssp));
      PROIO_yyvs = (YYSTYPE *) alloca (PROIO_yystacksize * sizeof (*PROIO_yyvsp));
      __PROIO_yy_bcopy ((char *)PROIO_yyvs1, (char *)PROIO_yyvs, size * sizeof (*PROIO_yyvsp));
#ifdef YYLSP_NEEDED
      PROIO_yyls = (YYLTYPE *) alloca (PROIO_yystacksize * sizeof (*PROIO_yylsp));
      __PROIO_yy_bcopy ((char *)PROIO_yyls1, (char *)PROIO_yyls, size * sizeof (*PROIO_yylsp));
#endif
#endif /* no PROIO_yyoverflow */

      PROIO_yyssp = PROIO_yyss + size - 1;
      PROIO_yyvsp = PROIO_yyvs + size - 1;
#ifdef YYLSP_NEEDED
      PROIO_yylsp = PROIO_yyls + size - 1;
#endif

#if YYDEBUG != 0
      if (PROIO_yydebug)
	fprintf(stderr, "Stack size increased to %d\n", PROIO_yystacksize);
#endif

      if (PROIO_yyssp >= PROIO_yyss + PROIO_yystacksize - 1)
	YYABORT;
    }

#if YYDEBUG != 0
  if (PROIO_yydebug)
    fprintf(stderr, "Entering state %d\n", PROIO_yystate);
#endif

  goto PROIO_yybackup;
 PROIO_yybackup:

/* Do appropriate processing given the current state.  */
/* Read a lookahead token if we need one and don't already have one.  */
/* PROIO_yyresume: */

  /* First try to decide what to do without reference to lookahead token.  */

  PROIO_yyn = PROIO_yypact[PROIO_yystate];
  if (PROIO_yyn == YYFLAG)
    goto PROIO_yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* PROIO_yychar is either YYEMPTY or YYEOF
     or a valid token in external form.  */

  if (PROIO_yychar == YYEMPTY)
    {
#if YYDEBUG != 0
      if (PROIO_yydebug)
	fprintf(stderr, "Reading a token: ");
#endif
      PROIO_yychar = YYLEX;
    }

  /* Convert token to internal form (in PROIO_yychar1) for indexing tables with */

  if (PROIO_yychar <= 0)		/* This means end of PROIO_input. */
    {
      PROIO_yychar1 = 0;
      PROIO_yychar = YYEOF;		/* Don't call YYLEX any more */

#if YYDEBUG != 0
      if (PROIO_yydebug)
	fprintf(stderr, "Now at end of PROIO_input.\n");
#endif
    }
  else
    {
      PROIO_yychar1 = YYTRANSLATE(PROIO_yychar);

#if YYDEBUG != 0
      if (PROIO_yydebug)
	{
	  fprintf (stderr, "Next token is %d (%s", PROIO_yychar, PROIO_yytname[PROIO_yychar1]);
	  /* Give the individual parser a way to print the precise meaning
	     of a token, for further debugging info.  */
#ifdef YYPRINT
	  YYPRINT (stderr, PROIO_yychar, PROIO_yylval);
#endif
	  fprintf (stderr, ")\n");
	}
#endif
    }

  PROIO_yyn += PROIO_yychar1;
  if (PROIO_yyn < 0 || PROIO_yyn > YYLAST || PROIO_yycheck[PROIO_yyn] != PROIO_yychar1)
    goto PROIO_yydefault;

  PROIO_yyn = PROIO_yytable[PROIO_yyn];

  /* PROIO_yyn is what to do for this token type in this state.
     Negative => reduce, -PROIO_yyn is rule number.
     Positive => shift, PROIO_yyn is new state.
       New state is final state => don't bother to shift,
       just return success.
     0, or most negative number => error.  */

  if (PROIO_yyn < 0)
    {
      if (PROIO_yyn == YYFLAG)
	goto PROIO_yyerrlab;
      PROIO_yyn = -PROIO_yyn;
      goto PROIO_yyreduce;
    }
  else if (PROIO_yyn == 0)
    goto PROIO_yyerrlab;

  if (PROIO_yyn == YYFINAL)
    YYACCEPT;

  /* Shift the lookahead token.  */

#if YYDEBUG != 0
  if (PROIO_yydebug)
    fprintf(stderr, "Shifting token %d (%s), ", PROIO_yychar, PROIO_yytname[PROIO_yychar1]);
#endif

  /* Discard the token being shifted unless it is eof.  */
  if (PROIO_yychar != YYEOF)
    PROIO_yychar = YYEMPTY;

  *++PROIO_yyvsp = PROIO_yylval;
#ifdef YYLSP_NEEDED
  *++PROIO_yylsp = PROIO_yylloc;
#endif

  /* count tokens shifted since error; after three, turn off error status.  */
  if (PROIO_yyerrstatus) PROIO_yyerrstatus--;

  PROIO_yystate = PROIO_yyn;
  goto PROIO_yynewstate;

/* Do the default action for the current state.  */
PROIO_yydefault:

  PROIO_yyn = PROIO_yydefact[PROIO_yystate];
  if (PROIO_yyn == 0)
    goto PROIO_yyerrlab;

/* Do a reduction.  PROIO_yyn is the number of a rule to reduce with.  */
PROIO_yyreduce:
  PROIO_yylen = PROIO_yyr2[PROIO_yyn];
  if (PROIO_yylen > 0)
    PROIO_yyval = PROIO_yyvsp[1-PROIO_yylen]; /* implement default value of the action */

#if YYDEBUG != 0
  if (PROIO_yydebug)
    {
      int i;

      fprintf (stderr, "Reducing via rule %d (line %d), ",
	       PROIO_yyn, PROIO_yyrline[PROIO_yyn]);

      /* Print the symbols being reduced, and their result.  */
      for (i = PROIO_yyprhs[PROIO_yyn]; PROIO_yyrhs[i] > 0; i++)
	fprintf (stderr, "%s ", PROIO_yytname[PROIO_yyrhs[i]]);
      fprintf (stderr, " -> %s\n", PROIO_yytname[PROIO_yyr1[PROIO_yyn]]);
    }
#endif


  switch (PROIO_yyn) {

case 3:
#line 66 "PrologIO/parser.y"
{ process_command(proio_cons(make_word(PROIO_yyvsp[-1].s), NULL)); free(PROIO_yyvsp[-1].s); ;
    break;}
case 4:
#line 68 "PrologIO/parser.y"
{ process_command(PROIO_yyvsp[-1].s); ;
    break;}
case 5:
#line 70 "PrologIO/parser.y"
{ syntax_error("Unrecognized command."); ;
    break;}
case 6:
#line 73 "PrologIO/parser.y"
{ PROIO_yyval.s = proio_cons(make_word(PROIO_yyvsp[-3].s), PROIO_yyvsp[-1].s); free(PROIO_yyvsp[-3].s); ;
    break;}
case 7:
#line 75 "PrologIO/parser.y"
{ PROIO_yyval.s = proio_cons(NULL, NULL); ;
    break;}
case 8:
#line 77 "PrologIO/parser.y"
{ PROIO_yyval.s = PROIO_yyvsp[-1].s; ;
    break;}
case 9:
#line 80 "PrologIO/parser.y"
{ PROIO_yyval.s = NULL; ;
    break;}
case 10:
#line 82 "PrologIO/parser.y"
{ PROIO_yyval.s = proio_cons(PROIO_yyvsp[0].s, NULL); ;
    break;}
case 11:
#line 84 "PrologIO/parser.y"
{ PROIO_yyval.s = proio_cons(PROIO_yyvsp[-2].s, PROIO_yyvsp[0].s); ;
    break;}
case 12:
#line 87 "PrologIO/parser.y"
{
		      PROIO_yyval.s = proio_cons(make_word("="),
				      proio_cons(make_word(PROIO_yyvsp[-2].s), proio_cons(PROIO_yyvsp[0].s, NULL)));
		      free(PROIO_yyvsp[-2].s);
		  ;
    break;}
case 13:
#line 93 "PrologIO/parser.y"
{ PROIO_yyval.s = PROIO_yyvsp[0].s; ;
    break;}
case 14:
#line 96 "PrologIO/parser.y"
{PROIO_yyval.s = make_word(PROIO_yyvsp[0].s); free(PROIO_yyvsp[0].s);;
    break;}
case 15:
#line 98 "PrologIO/parser.y"
{PROIO_yyval.s = make_string(PROIO_yyvsp[0].s); free(PROIO_yyvsp[0].s);;
    break;}
case 16:
#line 100 "PrologIO/parser.y"
{PROIO_yyval.s = make_integer(PROIO_yyvsp[0].s); free(PROIO_yyvsp[0].s);;
    break;}
case 17:
#line 102 "PrologIO/parser.y"
{PROIO_yyval.s = make_real(PROIO_yyvsp[-2].s, PROIO_yyvsp[0].s); free(PROIO_yyvsp[-2].s); free(PROIO_yyvsp[0].s); ;
    break;}
case 18:
#line 104 "PrologIO/parser.y"
{PROIO_yyval.s = make_exp(PROIO_yyvsp[-2].s, PROIO_yyvsp[0].s); free(PROIO_yyvsp[-2].s); free(PROIO_yyvsp[0].s); ;
    break;}
case 19:
#line 106 "PrologIO/parser.y"
{PROIO_yyval.s = make_exp2(PROIO_yyvsp[-4].s, PROIO_yyvsp[-2].s, PROIO_yyvsp[0].s); free(PROIO_yyvsp[-4].s); free(PROIO_yyvsp[-2].s); free(PROIO_yyvsp[0].s); ;
    break;}
case 20:
#line 108 "PrologIO/parser.y"
{PROIO_yyval.s = PROIO_yyvsp[0].s;;
    break;}
}
   /* the action file gets copied in in place of this dollarsign */
#line 465 "/usr/lib/bison.simple"

  PROIO_yyvsp -= PROIO_yylen;
  PROIO_yyssp -= PROIO_yylen;
#ifdef YYLSP_NEEDED
  PROIO_yylsp -= PROIO_yylen;
#endif

#if YYDEBUG != 0
  if (PROIO_yydebug)
    {
      short *ssp1 = PROIO_yyss - 1;
      fprintf (stderr, "state stack now");
      while (ssp1 != PROIO_yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

  *++PROIO_yyvsp = PROIO_yyval;

#ifdef YYLSP_NEEDED
  PROIO_yylsp++;
  if (PROIO_yylen == 0)
    {
      PROIO_yylsp->first_line = PROIO_yylloc.first_line;
      PROIO_yylsp->first_column = PROIO_yylloc.first_column;
      PROIO_yylsp->last_line = (PROIO_yylsp-1)->last_line;
      PROIO_yylsp->last_column = (PROIO_yylsp-1)->last_column;
      PROIO_yylsp->text = 0;
    }
  else
    {
      PROIO_yylsp->last_line = (PROIO_yylsp+PROIO_yylen-1)->last_line;
      PROIO_yylsp->last_column = (PROIO_yylsp+PROIO_yylen-1)->last_column;
    }
#endif

  /* Now "shift" the result of the reduction.
     Determine what state that goes to,
     based on the state we popped back to
     and the rule number reduced by.  */

  PROIO_yyn = PROIO_yyr1[PROIO_yyn];

  PROIO_yystate = PROIO_yypgoto[PROIO_yyn - YYNTBASE] + *PROIO_yyssp;
  if (PROIO_yystate >= 0 && PROIO_yystate <= YYLAST && PROIO_yycheck[PROIO_yystate] == *PROIO_yyssp)
    PROIO_yystate = PROIO_yytable[PROIO_yystate];
  else
    PROIO_yystate = PROIO_yydefgoto[PROIO_yyn - YYNTBASE];

  goto PROIO_yynewstate;

PROIO_yyerrlab:   /* here on detecting error */

  if (! PROIO_yyerrstatus)
    /* If not already recovering from an error, report this error.  */
    {
      ++PROIO_yynerrs;

#ifdef YYERROR_VERBOSE
      PROIO_yyn = PROIO_yypact[PROIO_yystate];

      if (PROIO_yyn > YYFLAG && PROIO_yyn < YYLAST)
	{
	  int size = 0;
	  char *msg;
	  int x, count;

	  count = 0;
	  /* Start X at -PROIO_yyn if nec to avoid negative indexes in PROIO_yycheck.  */
	  for (x = (PROIO_yyn < 0 ? -PROIO_yyn : 0);
	       x < (sizeof(PROIO_yytname) / sizeof(char *)); x++)
	    if (PROIO_yycheck[x + PROIO_yyn] == x)
	      size += strlen(PROIO_yytname[x]) + 15, count++;
	  msg = (char *) malloc(size + 15);
	  if (msg != 0)
	    {
	      strcpy(msg, "parse error");

	      if (count < 5)
		{
		  count = 0;
		  for (x = (PROIO_yyn < 0 ? -PROIO_yyn : 0);
		       x < (sizeof(PROIO_yytname) / sizeof(char *)); x++)
		    if (PROIO_yycheck[x + PROIO_yyn] == x)
		      {
			strcat(msg, count == 0 ? ", expecting `" : " or `");
			strcat(msg, PROIO_yytname[x]);
			strcat(msg, "'");
			count++;
		      }
		}
	      PROIO_yyerror(msg);
	      free(msg);
	    }
	  else
	    PROIO_yyerror ("parse error; also virtual memory exceeded");
	}
      else
#endif /* YYERROR_VERBOSE */
	PROIO_yyerror("parse error");
    }

  goto PROIO_yyerrlab1;
PROIO_yyerrlab1:   /* here on error raised explicitly by an action */

  if (PROIO_yyerrstatus == 3)
    {
      /* if just tried and failed to reuse lookahead token after an error, discard it.  */

      /* return failure if at end of PROIO_input */
      if (PROIO_yychar == YYEOF)
	YYABORT;

#if YYDEBUG != 0
      if (PROIO_yydebug)
	fprintf(stderr, "Discarding token %d (%s).\n", PROIO_yychar, PROIO_yytname[PROIO_yychar1]);
#endif

      PROIO_yychar = YYEMPTY;
    }

  /* Else will try to reuse lookahead token
     after shifting the error token.  */

  PROIO_yyerrstatus = 3;		/* Each real token shifted decrements this */

  goto PROIO_yyerrhandle;

PROIO_yyerrdefault:  /* current state does not do anything special for the error token. */

#if 0
  /* This is wrong; only states that explicitly want error tokens
     should shift them.  */
  PROIO_yyn = PROIO_yydefact[PROIO_yystate];  /* If its default is to accept any token, ok.  Otherwise pop it.*/
  if (PROIO_yyn) goto PROIO_yydefault;
#endif

PROIO_yyerrpop:   /* pop the current state because it cannot handle the error token */

  if (PROIO_yyssp == PROIO_yyss) YYABORT;
  PROIO_yyvsp--;
  PROIO_yystate = *--PROIO_yyssp;
#ifdef YYLSP_NEEDED
  PROIO_yylsp--;
#endif

#if YYDEBUG != 0
  if (PROIO_yydebug)
    {
      short *ssp1 = PROIO_yyss - 1;
      fprintf (stderr, "Error: state stack now");
      while (ssp1 != PROIO_yyssp)
	fprintf (stderr, " %d", *++ssp1);
      fprintf (stderr, "\n");
    }
#endif

PROIO_yyerrhandle:

  PROIO_yyn = PROIO_yypact[PROIO_yystate];
  if (PROIO_yyn == YYFLAG)
    goto PROIO_yyerrdefault;

  PROIO_yyn += YYTERROR;
  if (PROIO_yyn < 0 || PROIO_yyn > YYLAST || PROIO_yycheck[PROIO_yyn] != YYTERROR)
    goto PROIO_yyerrdefault;

  PROIO_yyn = PROIO_yytable[PROIO_yyn];
  if (PROIO_yyn < 0)
    {
      if (PROIO_yyn == YYFLAG)
	goto PROIO_yyerrpop;
      PROIO_yyn = -PROIO_yyn;
      goto PROIO_yyreduce;
    }
  else if (PROIO_yyn == 0)
    goto PROIO_yyerrpop;

  if (PROIO_yyn == YYFINAL)
    YYACCEPT;

#if YYDEBUG != 0
  if (PROIO_yydebug)
    fprintf(stderr, "Shifting error token, ");
#endif

  *++PROIO_yyvsp = PROIO_yylval;
#ifdef YYLSP_NEEDED
  *++PROIO_yylsp = PROIO_yylloc;
#endif

  PROIO_yystate = PROIO_yyn;
  goto PROIO_yynewstate;
}
#line 111 "PrologIO/parser.y"


#ifdef wx_xt
#	include <./PrologIO/lexer.c>
#else
#	include <lexer.c>
#endif

void PROIO_yyerror(char *s)
{
    syntax_error(s);
}

int PROIO_yywrap(void)
{
    return 1;
}
