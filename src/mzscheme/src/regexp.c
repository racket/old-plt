/*
 * @(#)regexp.c	1.3 of 18 April 87
 * Revised for PLT MzScheme, 1995-2001
 *
 *	Copyright (c) 1986 by University of Toronto.
 *	Written by Henry Spencer.  Not derived from licensed software.
 *
 *	Permission is granted to anyone to use this software for any
 *	purpose on any computer system, and to redistribute it freely,
 *	subject to the following restrictions:
 *
 *	1. The author is not responsible for the consequences of use of
 *		this software, no matter how awful, even if they arise
 *		from defects in it.
 *
 *	2. The origin of this software must not be misrepresented, either
 *		by explicit claim or by omission.
 *
 *	3. Altered versions must be plainly marked as such, and must not
 *		be misrepresented as being the original software.
 *
 * Beware that some of this code is subtly aware of the way operator
 * precedence is structured in regular expressions.  Serious changes in
 * regular-expression syntax might require a total rethink.
 *
 * Notable changes for MzScheme:
 *   Removed hardwired limits on parenthesis nesting
 *   Changed to index-based instead of pointer-based (better for GC)
 *   Added non-greedy operators *?, +?, and ??
 *   Added MzScheme glue
 *
 * from Vladimir Tsyshevsky:
 *  additional optional parameter `offset' in `regexp-match'
 *  and `regexp-match-positions'
 */

#include "schpriv.h"
#include "schmach.h"

#ifndef NO_REGEXP_UTILS

#include <stdio.h>
#include <string.h>

typedef long rxpos;
#ifdef SIXTY_FOUR_BIT_INTEGERS
# define BIGGEST_RXPOS 0x7FFFFFFFFFFFFFFF
#else
# define BIGGEST_RXPOS 0x7FFFFFFF
#endif

/* #define INDIRECT_TO_PROGRAM */

typedef struct regexp {
  Scheme_Type type;
  MZ_HASH_KEY_EX
  Scheme_Object *source;
  long nsubexp;
  long regsize;
  char regstart;		/* Internal use only. */
  char reganch;			/* Internal use only. */
  long regmust;                 /* Internal use only: => pointer relative to self */
  long regmlen;			/* Internal use only. */
#ifdef INDIRECT_TO_PROGRAM
  char *program;
#else
  char program[1];		/* Unwarranted chumminess with compiler. */
#endif
} regexp;

#ifdef INDIRECT_TO_PROGRAM
# define N_ITO_DELTA(prog, extra, re) extra
# define N_ITO_SPACE(v) 0
# define ITO(x, y) x
#else
# define N_ITO_DELTA(prog, extra, re) ((prog+extra) - re)
# define N_ITO_SPACE(v) v
# define ITO(x, y) y
#endif

static regexp *regcomp(char *, rxpos, int);
/* static int regexec(regexp *, char *, int, int, rxpos *, rxpos * ...); */

/* 156 is octal 234: */
#define MAGIC   156

#ifdef MZ_PRECISE_GC
# define MZREGISTER /**/
#else
# define MZREGISTER register
#endif

/*
 * The "internal use only" fields in regexp.h are present to pass info from
 * compile to execute that permits the execute phase to run lots faster on
 * simple cases.  They are:
 *
 * regstart	char that must begin a match; '\0' if none obvious
 * reganch	is the match anchored (at beginning-of-line only)?
 * regmust	string (pointer into program) that match must include, or NULL
 * regmlen	length of regmust string
 *
 * Regstart and reganch permit very fast decisions on suitable starting points
 * for a match, cutting down the work a lot.  Regmust permits fast rejection
 * of lines that cannot possibly match.  The regmust tests are costly enough
 * that regcomp() supplies a regmust only if the r.e. contains something
 * potentially expensive (at present, the only such thing detected is * or +
 * at the start of the r.e., which can involve a lot of backup).  Regmlen is
 * supplied because the test in regexec() needs it and regcomp() is computing
 * it anyway.
 */

/*
 * Structure for regexp "program".  This is essentially a linear encoding
 * of a nondeterministic finite-state machine (aka syntax charts or
 * "railroad normal form" in parsing technology).  Each node is an opcode
 * plus a "next" pointer, possibly plus an operand.  "Next" pointers of
 * all nodes except BRANCH implement concatenation; a "next" pointer with
 * a BRANCH on both ends of it is connecting two alternatives.  (Here we
 * have one of the subtle syntax dependencies:  an individual BRANCH (as
 * opposed to a collection of them) is never concatenated with anything
 * because of operator precedence.)  The operand of some types of node is
 * a literal string; for others, it is a node leading into a sub-FSM.  In
 * particular, the operand of a BRANCH node is the first node of the branch.
 * (NB this is *not* a tree structure:  the tail of the branch connects
 * to the thing following the set of BRANCHes.)  The opcodes are:
 */

/* definition	number	opnd?	meaning */
#define	END	0	/* no	End of program. */
#define	BOL	1	/* no	Match "" at beginning of line. */
#define	EOL	2	/* no	Match "" at end of line. */
#define	ANY	3	/* no	Match any one character. */
#define	ANYOF	4	/* str	Match any character in this string. */
#define	ANYBUT	5	/* str	Match any character not in this string. */
#define	BRANCH	6	/* node	Match this alternative, or the next... */
#define	BACK	7	/* no	Match "", "next" ptr points backward. */
#define	EXACTLY	8	/* str	Match this string. */
#define	NOTHING	9	/* no	Match empty string. */
#define	STAR	10	/* node	Match this (simple) thing 0 or more times. */
#define	PLUS	11	/* node	Match this (simple) thing 1 or more times. */
#define	STAR2	12	/* non-greedy star. */
#define	PLUS2	13	/* non-greedy plus. */
#define OPENN   14      /* like OPEN, but with an n >= 50, or n == 0 means (?:...) */
#define CLOSEN  15      /* like CLOSE, but with an n >= 50 */
#define	OPEN	20	/* no	Mark this point in input as start of #n. */
/*	OPEN+1 is number 1, etc. */
#define	CLOSE	70	/* no	Analogous to OPEN. */

# define OPSTR(o) (o + 2)
# define OPLEN(o) ((int)(((unsigned char *)regstr)[o] << 8) | (((unsigned char *)regstr)[o+1]))

/*
 * Opcode notes:
 *
 * BRANCH	The set of branches constituting a single choice are hooked
 *		together with their "next" pointers, since precedence prevents
 *		anything being concatenated to any individual branch.  The
 *		"next" pointer of the last BRANCH in a choice points to the
 *		thing following the whole choice.  This is also where the
 *		final "next" pointer of each individual branch points; each
 *		branch starts with the operand node of a BRANCH node.
 *
 * BACK		Normal "next" pointers all implicitly point forward; BACK
 *		exists to make loop structures possible.
 *
 * STAR,PLUS	'?', and complex '*' and '+', are implemented as circular
 *		BRANCH structures using BACK.  Simple cases (one character
 *		per match) are implemented with STAR and PLUS for speed
 *		and to minimize recursive plunges.
 *
 * OPEN,CLOSE	...are numbered at compile time.
 */

/*
 * A node is one char of opcode followed by two chars of "next" pointer.
 * "Next" pointers are stored as two 8-bit pieces, high order first.  The
 * value is a positive offset from the opcode of the node containing it.
 * An operand, if any, simply follows the node.  (Note that much of the
 * code generation knows about this implicit relationship.)
 *
 * Using two bytes for the "next" pointer is vast overkill for most things,
 * but allows patterns to get big without disasters.
 */
#define	OP(p)	(regstr[p])
#define	NEXT(p)	(((((unsigned char *)regstr)[(p)+1]&255)<<8) + (((unsigned char *)regstr)[(p)+2]&255))
#define	OPERAND(p)	((p) + 3)

/*
 * See regmagic.h for one further detail of program structure.
 */


/*
 * Utility definitions.
 */
#ifndef CHARBITS
#define	UCHARAT(p)	((int)*(unsigned char *)(p))
#else
#define	UCHARAT(p)	((int)*(p)&CHARBITS)
#endif

#define	FAIL(m)	{ regcomperror(m); return 0; }
#define	ISMULT(c)	((c) == '*' || (c) == '+' || (c) == '?')
#define	META	"^$.[()|?+*\\"

/*
 * Flags to be passed up and down.
 */
#define	HASWIDTH	01	/* Known never to match null string. */
#define	SIMPLE		02	/* Simple enough to be STAR/PLUS operand. */
#define	SPSTART		04	/* Starts with * or +. */
#define	WORST		0	/* Worst case. */

/*
 * Global work variables for regcomp().
 */
static char *regstr, *regparsestr;

static rxpos regparse, regparse_end; /* Input-scan pointer. */
static int regnpar;		/* () count. */
static char regdummy;
static rxpos regcode;		/* Code-emit pointer; &regdummy = don't. */
static long regsize;		/* Code size. */

#define REGDUMMY &regdummy

/*
 * Forward declarations for regcomp()'s friends.
 */
#ifndef STATIC
#define	STATIC	static
#endif
STATIC rxpos reg(int, int *, int);
STATIC rxpos regbranch(int *);
STATIC rxpos regpiece(int *);
STATIC rxpos regatom(int *);
STATIC rxpos regnode(char);
STATIC rxpos regnext(rxpos);
STATIC void regc(char);
STATIC void reginsert(char, rxpos);
STATIC void regtail(rxpos, rxpos);
STATIC void regoptail(rxpos, rxpos);
STATIC int regstrcspn(char *, char *, char *);

static void
regerror(char *s)
{
  scheme_raise_exn(MZEXN_MISC,
		   "regexp: %s", s);
}

const char *failure_msg_for_read;

static void
regcomperror(char *s)
{
  if (failure_msg_for_read) {
    failure_msg_for_read = s;
    scheme_longjmp(scheme_error_buf, 1);
  } else
    regerror(s);
}

/*
 - regcomp - compile a regular expression into internal code
 *
 * We can't allocate space until we know how big the compiled form will be,
 * but we can't compile it (and thus know how big it is) until we've got a
 * place to put the code.  So we cheat:  we compile it twice, once with code
 * generation turned off and size counting turned on, and once "for real".
 * This also means that we don't allocate space until we are sure that the
 * thing really will compile successfully, and we never have to move the
 * code and thus invalidate pointers into it.  (Note that it has to be in
 * one piece because free() must be able to free it all.)
 *
 * Beware that the optimization-preparation code in here knows about some
 * of the structure of the compiled regexp.
 */
static regexp *
regcomp(char *expstr, rxpos exp, int explen)
{
  regexp *r;
  rxpos scan, next;
  rxpos longest;
  int len;
  int flags;
  
  /* First pass: determine size, legality. */
  regstr = REGDUMMY;
  regparsestr = expstr;
  regparse = exp;
  regparse_end = exp + explen;
  regnpar = 1;
  regsize = 0L;
  regcode = 1;
  regc(MAGIC);
  if (reg(0, &flags, 0) == 0)
    return NULL;
  
  /* Small enough for pointer-storage convention? */
  if (regsize >= 32767L)		/* Probably could be 65535L. */
    FAIL("regexp too big");
  
  /* Allocate space. */
  r = (regexp *)scheme_malloc_tagged(sizeof(regexp) + N_ITO_SPACE((unsigned)regsize));
  r->type = scheme_regexp_type;
  
#ifdef INDIRECT_TO_PROGRAM
  r->program = (char *)scheme_malloc_atomic((unsigned)regsize + 1);
#endif
  
  r->regsize = regsize;

  r->nsubexp = regnpar;
  
  /* Second pass: emit code. */
  regparse = exp;
  regparse_end = exp + explen;
  regnpar = 1;
#ifdef INDIRECT_TO_PROGRAM
  regstr = r->program;
  regcode = 0;
#else
  regstr = (char *)r;
  regcode = (char *)r->program - (char *)r;
#endif
  regc(MAGIC);
  if (reg(0, &flags, 0) == 0)
    return NULL;
  
  /* Dig out information for optimizations. */
  r->regstart = '\0';	/* Worst-case defaults. */
  r->reganch = 0;
  r->regmust = -1;
  r->regmlen = 0;
  scan = N_ITO_DELTA(r->program, 1, (char *)r);    /* First BRANCH. */
  next = regnext(scan);
  if (OP(next) == END) {	/* Only one top-level choice. */
    scan = OPERAND(scan);
    
    /* Starting-point info. */
    if (OP(scan) == EXACTLY)
      r->regstart = regstr[OPSTR(OPERAND(scan))];
    else if (OP(scan) == BOL)
      r->reganch++;
    
    /*
     * If there's something expensive in the r.e., find the
     * longest literal string that must appear and make it the
     * regmust.  Resolve ties in favor of later strings, since
     * the regstart check works with the beginning of the r.e.
     * and avoiding duplication strengthens checking.  Not a
     * strong reason, but sufficient in the absence of others.
     */
    if (flags&SPSTART) {
      longest = 0;
      len = 0;
      for (; scan != 0; scan = regnext(scan)) {
	if (OP(scan) == EXACTLY && OPLEN(OPERAND(scan)) >= len) {
	  /* Skip regmust if it contains a null character: */
	  rxpos ls = OPSTR(OPERAND(scan));
	  int ll = OPLEN(OPERAND(scan)), i;
	  for (i = 0; i < ll; i++) {
	    if (!regstr[ls + i])
	      break;
	  }
	  if (i >= ll) {
	    longest = ls;
	    len = ll;
	  }
	}
      }
      if (longest)
	r->regmust = longest;
      r->regmlen = len;
    }
  }

#if 0
  if (regcode > r->regsize + sizeof(regexp))
    scheme_signal_error("regexp too large!");
#endif
  
  return(r);
}

#ifdef DO_STACK_CHECK

static Scheme_Object *reg_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  int *flagp = (int *)p->ku.k.p1;
  int res;

  p->ku.k.p1 = NULL;

  res = reg(p->ku.k.i1, flagp, p->ku.k.i2);

  return scheme_make_integer(res);
}

#endif

/*
   - reg - regular expression, i.e. main body or parenthesized thing
   *
   * Caller must absorb opening parenthesis.
   *
   * Combining parenthesis handling with the base level of regular expression
   * is a trifle forced, but the need to tie the tails of the branches to what
   * follows makes it hard to avoid.
   */
static rxpos
reg(int paren, int *flagp, int paren_set)
{
  rxpos ret;
  rxpos br;
  rxpos ender;
  int parno = 0;
  int flags;

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Scheme_Object *ov;
      p->ku.k.i1 = paren;
      p->ku.k.p1 = (void *)flagp;
      p->ku.k.i2 = paren_set;
      ov = scheme_handle_stack_overflow(reg_k);
      return SCHEME_INT_VAL(ov);
    }
  }
#endif

  *flagp = HASWIDTH;		/* Tentatively. */

  /* Make an OPEN node, if parenthesized. */
  if (paren) {
    if (paren_set) {
      parno = regnpar;
      regnpar++;
    } else
      parno = 0;
    if (!paren_set || (OPEN + parno >= CLOSE)) {
      ret = regcode;
      regc(parno >> 8);
      regc(parno & 255);
      reginsert(OPENN, ret);
    } else {
      ret = regnode(OPEN+parno);
    }
  } else
    ret = 0;

  /* Pick up the branches, linking them together. */
  br = regbranch(&flags);
  if (br == 0)
    return 0;
  if (ret != 0)
    regtail(ret, br);		/* OPEN -> first. */
  else
    ret = br;
  if (!(flags&HASWIDTH))
    *flagp &= ~HASWIDTH;
  *flagp |= flags&SPSTART;
  while (regparsestr[regparse] == '|') {
    regparse++;
    br = regbranch(&flags);
    if (br == 0)
      return 0;
    regtail(ret, br);		/* BRANCH -> BRANCH. */
    if (!(flags&HASWIDTH))
      *flagp &= ~HASWIDTH;
    *flagp |= flags&SPSTART;
  }

  /* Make a closing node, and hook it on the end. */
  if (paren) {
    if (!paren_set || (OPEN + parno >= CLOSE)) {
      ender = regcode;
      regc(parno >> 8);
      regc(parno & 255);
      reginsert(CLOSEN, ender);
    } else
      ender = regnode(CLOSE+parno);
  } else {
    ender = regnode(END);	
  }
  regtail(ret, ender);

  /* Hook the tails of the branches to the closing node. */
  for (br = ret; br != 0; br = regnext(br)) {
    regoptail(br, ender);
  }

  /* Check for proper termination. */
  if (paren && regparsestr[regparse++] != ')') {
    FAIL("missing closing parenthesis in pattern");
  } else if (!paren && regparse != regparse_end) {
    if (regparsestr[regparse] == ')') {
      FAIL("extra closing parenthesis in pattern");
    } else
      FAIL("junk on end");	/* "Can't happen". */
    /* NOTREACHED */
  }

  return ret;
}

/*
   - regbranch - one alternative of an | operator
   *
   * Implements the concatenation operator.
   */
static rxpos
regbranch(int *flagp)
{
  rxpos ret;
  rxpos chain;
  rxpos latest;
  int flags;

  *flagp = WORST;		/* Tentatively. */

  ret = regnode(BRANCH);
  chain = 0;
  while (regparse != regparse_end 
	 && regparsestr[regparse] != '|' 
	 && regparsestr[regparse] != ')') {
    latest = regpiece(&flags);
    if (latest == 0)
      return 0;
    *flagp |= flags&HASWIDTH;
    if (chain == 0)		/* First piece. */
      *flagp |= flags&SPSTART;
    else
      regtail(chain, latest);
    chain = latest;
  }
  if (chain == 0)		/* Loop ran zero times. */
    (void) regnode(NOTHING);

  return(ret);
}

/*
   - regpiece - something followed by possible [*+?]
   *
   * Note that the branching code sequences used for ? and the general cases
   * of * and + are somewhat optimized:  they use the same NOTHING node as
   * both the endmarker for their branch list and the body of the last branch.
   * It might seem that this node could be dispensed with entirely, but the
   * endmarker role is not redundant.
   */
static rxpos 
regpiece(int *flagp)
{
  rxpos ret;
  char op;
  rxpos next;
  int flags, greedy;

  ret = regatom(&flags);
  if (ret == 0)
    return 0;

  op = regparsestr[regparse];
  if (!ISMULT(op)) {
    *flagp = flags;
    return(ret);
  }

  if (!(flags&HASWIDTH) && op != '?')
    FAIL("* or + operand could be empty");
  *flagp = (op != '+') ? (WORST|SPSTART) : (WORST|HASWIDTH);

  if (regparsestr[regparse+1] == '?') {
    greedy = 0;
    regparse++;
  } else
    greedy = 1;

  if (op == '*' && (flags&SIMPLE))
    reginsert(greedy ? STAR : STAR2, ret);
  else if (op == '*' && greedy) {
    /* Emit x* as (x&|), where & means "self". */
    reginsert(BRANCH, ret);	/* Either x */
    regoptail(ret, regnode(BACK)); /* and loop */
    regoptail(ret, ret);	/* back */
    regtail(ret, regnode(BRANCH)); /* or */
    regtail(ret, regnode(NOTHING)); /* null. */
  } else if (op == '*') {
    /* Emit x*? as (|x&), where & means "self". */
    reginsert(BRANCH, ret);  /* will be next... */
    reginsert(NOTHING, ret);
    reginsert(BRANCH, ret);
    next = ret + 6;
    regtail(ret, next);
    regoptail(next, regnode(BACK)); /* loop */
    regoptail(next, ret);	/* back. */
    regtail(next, regnode(BACK));
    regtail(next, ret + 3);
  } else if (op == '+' && (flags&SIMPLE))
    reginsert(greedy ? PLUS : PLUS2, ret);
  else if (op == '+' && greedy) {
    /* Emit x+ as x(&|), where & means "self". */
    next = regnode(BRANCH);	/* Either */
    regtail(ret, next);
    regtail(regnode(BACK), ret); /* loop back */
    regtail(next, regnode(BRANCH)); /* or */
    regtail(ret, regnode(NOTHING)); /* null. */
  } else if (op == '+') {
    /* Emit x+? as x(|&), where & means "self". */
    next = regnode(BRANCH);	/* Either */
    regtail(ret, next);
    regnode(NOTHING); /* op */
    regtail(next, regnode(BRANCH)); /* or */
    regtail(regnode(BACK), ret); /* loop back. */
    regtail(next, regnode(BACK));
    regtail(next, next + 3);
  } else if (op == '?' && greedy) {
    /* Emit x? as (x|) */
    reginsert(BRANCH, ret);	/* Either x */
    regtail(ret, regnode(BRANCH)); /* or */
    next = regnode(NOTHING);	/* null. */
    regtail(ret, next);
    regoptail(ret, next);
  } else if (op == '?') {
    /* Emit x?? as (|x) */
    reginsert(BRANCH, ret);  /* will be next... */
    reginsert(NOTHING, ret);
    reginsert(BRANCH, ret);
    regtail(ret, ret + 6);
    next = regnode(BACK);
    regtail(ret + 6, next);
    regoptail(ret + 6, next);
    regoptail(ret + 6, ret + 3);
  }
  regparse++;
  if (ISMULT(regparsestr[regparse]))
    FAIL("nested *, ?, or + in pattern");

  return(ret);
}

/*
   - regatom - the lowest level
   *
   * Optimization:  gobbles an entire sequence of ordinary characters so that
   * it can turn them into a single node, which is smaller to store and
   * faster to run.  Backslashed characters are exceptions, each becoming a
   * separate node; the code is simpler that way and it's not worth fixing.
 */
static rxpos 
regatom(int *flagp)
{
  rxpos ret;
  int flags;

  *flagp = WORST;		/* Tentatively. */

  switch (regparsestr[regparse++]) {
  case '^':
    ret = regnode(BOL);
    break;
  case '$':
    ret = regnode(EOL);
    break;
  case '.':
    ret = regnode(ANY);
    *flagp |= HASWIDTH|SIMPLE;
    break;
  case '[': {
    int xclass;
    int classend, len;
    rxpos l0, l1;

    if (regparsestr[regparse] == '^') { /* Complement of range. */
      ret = regnode(ANYBUT);
      regparse++;
    } else
      ret = regnode(ANYOF);
    len = 0;
    l0 = regcode;
    regc(0);
    l1 = regcode;
    regc(0);
    if (regparsestr[regparse] == ']' || regparsestr[regparse] == '-') {
      regc(regparsestr[regparse++]);
      len++;
    }
    while (regparse != regparse_end && regparsestr[regparse] != ']') {
      if (regparsestr[regparse] == '-') {
	regparse++;
	if (regparsestr[regparse] == ']' || regparse == regparse_end) {
	  regc('-');
	  len++;
	} else {
	  xclass = UCHARAT(regparsestr + (regparse-2))+1;
	  classend = UCHARAT(regparsestr + regparse);
	  if (xclass > classend+1)
	    FAIL("invalid range within square brackets in pattern");
	  for (; xclass <= classend; xclass++) {
	    regc(xclass);
	    len++;
	  }
	  regparse++;
	}
      } else {
	regc(regparsestr[regparse++]);
	len++;
      }
    }
    if (regparsestr[regparse] != ']')
      FAIL("missing closing square bracket in pattern");
    regparse++;
    regstr[l0] = (len >> 8);
    regstr[l1] = (len & 255);
    *flagp |= HASWIDTH|SIMPLE;
  }
  break;
  case '(':
    if ((regparsestr[regparse] == '?')
	&& (regparsestr[regparse+1] == ':')) {
      regparse += 2;
      ret = reg(1, &flags, 0);
    } else
      ret = reg(1, &flags, 1);
    if (ret == 0)
      return 0;
    *flagp |= flags&(HASWIDTH|SPSTART);
    break;
  case '|':
  case ')':
    FAIL("internal urp");	/* Supposed to be caught earlier. */
    break;
  case '?':
  case '+':
  case '*':
    FAIL("?, +, or * follows nothing in pattern");
    break;
  case '\\':
    if (regparse == regparse_end)
      FAIL("trailing backslash in pattern");
    ret = regnode(EXACTLY);
    regc(0);
    regc(1);
    regc(regparsestr[regparse++]);
    *flagp |= HASWIDTH|SIMPLE;
    break;
  default: {
      int len;
      char ender;

      regparse--;
      len = regstrcspn(regparsestr + regparse, regparsestr + regparse_end, META);
      if (len <= 0)
	FAIL("internal disaster");
      ender = regparsestr[regparse+len];
      if (len > 1 && ISMULT(ender))
	len--;		/* Back off clear of ?+* operand. */
      *flagp |= HASWIDTH;
      if (len == 1)
	*flagp |= SIMPLE;
      ret = regnode(EXACTLY);
      regc(len >> 8); regc(len & 255);
      while (len > 0) {
	regc(regparsestr[regparse++]);
	len--;
      }
    }
    break;
  }
	
  return ret;
}

/*
   - regnode - emit a node
   */
static rxpos 			/* Location. */
regnode(char op)
{
  rxpos ret;
  rxpos ptr;

  ret = regcode;
  if (regstr == REGDUMMY) {
    regsize += 3;
    return ret;
  }

  ptr = ret;
  regstr[ptr++] = op;
  regstr[ptr++] = '\0';		/* Null "next" pointer. */
  regstr[ptr++] = '\0';
  regcode = ptr;

  return ret;
}

/*
   - regc - emit (if appropriate) a byte of code
   */
static void
regc(char b)
{
  if (regstr != REGDUMMY)
    regstr[regcode++] = b;
  else
    regsize++;
}

/*
   - reginsert - insert an operator in front of already-emitted operand
   *
   * Means relocating the operand.
   */
static void
reginsert(char op, rxpos opnd)
{
  rxpos src;
  rxpos dst;
  rxpos place;

  if (regstr == REGDUMMY) {
    regsize += 3;
    return;
  }

  src = regcode;
  regcode += 3;
  dst = regcode;
  while (src > opnd) {
    regstr[--dst] = regstr[--src];
  }

  place = opnd;			/* Op node, where operand used to be. */
  regstr[place++] = op;
  regstr[place++] = '\0';
  regstr[place++] = '\0';
}

/*
   - regtail - set the next-pointer at the end of a node chain
   */
static void
regtail(rxpos p, rxpos val)
{
  rxpos scan;
  rxpos temp;
  int offset;

  if (regstr == REGDUMMY)
    return;

  /* Find last node. */
  scan = p;
  for (;;) {
    temp = regnext(scan);
    if (temp == 0)
      break;
    scan = temp;
  }

  if (OP(scan) == BACK)
    offset = scan - val;
  else
    offset = val - scan;
  regstr[scan+1] = (offset>>8)&255;
  regstr[scan+2] = offset&255;
}

/*
   - regoptail - regtail on operand of first argument; nop if operandless
   */
static void
regoptail(rxpos p, rxpos val)
{
  /* "Operandless" and "op != BRANCH" are synonymous in practice. */
  if (p == 0 || regstr == REGDUMMY || OP(p) != BRANCH)
    return;
  regtail(OPERAND(p), val);
}

static rxpos l_strchr(char *str, rxpos a, int l, int c)
{
  int i;

  for (i = 0; i < l; i++) {
    if (str[a + i] == c)
      return a + i;
  }

  return -1;
}

/*
 * regexec and friends
 */

/*
 * Work variables for regtry().
 */
typedef struct Regwork {
  MZTAG_IF_REQUIRED
  char *str;              /* copy of regstr; used only to protect before thread swaps */
  char *instr;
  Scheme_Object *port;
  rxpos instr_size;       /* For port reads */
  rxpos input_maxend;     /* For port reads */
  rxpos input, input_end; /* String-input pointer. */
  rxpos bol;		  /* Beginning of input, for ^ check. */
  rxpos *startp;	  /* Pointer to startp array. */
  rxpos *endp;		  /* Ditto for endp. */
  Scheme_Object *peekskip;
} Regwork;

/*
 * Forwards.
 */
STATIC int regtry(regexp *, char *, int, int, rxpos *, rxpos *, Regwork *rw, int);
STATIC int regtry_port(regexp *, Scheme_Object *, rxpos *, rxpos *, 
		       char **, rxpos *, rxpos *, rxpos, Scheme_Object*, Scheme_Object*, int);
STATIC int regmatch(Regwork *rw, rxpos);
STATIC int regrepeat(Regwork *rw, rxpos);

#ifdef DEBUG
int regnarrate = 0;
void regdump();
STATIC char *regprop();
#endif

#define REGPORT_FLUSH_THRESHOLD 256

/*
   - regexec - match a regexp against a string
   */
static int
regexec(const char *who,
	regexp *prog, char *string, 
	/* used only for strings: */
	int stringpos, int stringlen, 
	/* Always used: */
	rxpos *startp, rxpos *endp,
	Scheme_Object *port, 
	/* Used only when port is non-NULL: */
	char **stringp, int peek, int get_offsets,
	Scheme_Object *discard_oport, 
	Scheme_Object *portstart, Scheme_Object *portend, Scheme_Object **_dropped)
{
  int spos;
  int slen;
  Scheme_Object *dropped = NULL, *peekskip = NULL; /* used for ports, only */
 
  /* Check validity of program. */
  if (UCHARAT(prog->program) != MAGIC) {
    regerror("corrupted program");
    return(0);
  }

  /* If there is a "must appear" string, look for it. */
  if (!port && (prog->regmust >= 0)) {
    spos = stringpos;
    slen = stringlen;
    while ((spos = l_strchr(string, spos, slen, (ITO(prog->program, (char *)prog) + prog->regmust)[0])) != -1) {
      int i, l = prog->regmlen;
      GC_CAN_IGNORE char *p = (ITO(prog->program, (char *)prog) + prog->regmust); /* ASSUMING NO GC HERE! */
      slen = stringlen - (spos - stringpos);
      for (i = 0; (i < l) && (i < slen); i++) {
	if (string[spos + i] != p[i])
	  break;
      }
      if (i >= l)
	break; /* Found it. */
      spos++;
      slen--;
    }
    if (spos == -1) /* Not present. */
      return 0;
  }

  if (port) {
    if (peek) {
      peekskip = portstart;
      dropped = portstart;
    } else {
      /* In non-peek port mode, skip over portstart chars: */
      long amt, got;

      if (SCHEME_INTP(portstart)) {
	amt = SCHEME_INT_VAL(portstart);
	if (amt > 4096)
	  amt = 4096;
      } else
	amt = 4096;

      dropped = scheme_make_integer(0);
	
      if (amt) {
	char *drain;

	drain = (char *)scheme_malloc_atomic(amt);

	do {
	  got = scheme_get_string(who, port, drain, 0, amt, 0, 0, 0);
	  if (got != EOF) {
	    Scheme_Object *delta;
	    
	    if (discard_oport)
	      scheme_put_string(who, discard_oport, drain, 0, got, 0);
	    
	    dropped = scheme_bin_plus(dropped, scheme_make_integer(amt));
	    delta = scheme_bin_minus(portstart, dropped);
	    if (scheme_bin_gt(scheme_make_integer(amt), delta))
	      amt = SCHEME_INT_VAL(delta);
	  }
	} while ((got != EOF) && amt);
	if (amt)
	  return 0; /* can't skip far enough, so it fails */
      }
    }

    if (portend)
      portend = scheme_bin_minus(portend, dropped);
  }

  /* Simplest case:  anchored match need be tried only once. */
  if (prog->reganch) {
    if (port) {
      rxpos len = 0, space = 0;

      *stringp = NULL;
      if (regtry_port(prog, port, startp, endp, stringp, &len, &space, 0, portend, peekskip, 1)) {
	if (!peek) {
	  /* Need to consume matched chars: */
	  char *drain;
	  long got;

	  if (discard_oport && *startp)
	    scheme_put_string(who, discard_oport, *stringp, 0, *startp, 0);

	  if (get_offsets)
	    drain = *stringp;
	  else
	    /* Allocate fresh in case we get different results from previous peek: */
	    drain = (char *)scheme_malloc_atomic(*endp);
	  got = scheme_get_string(who, port, drain, 0, *endp, 0, 0, 0);
	}

	*_dropped = dropped;

	return 1;
      } else {
	if (!peek) {
	  /* Need to consume all chars */
	  char *drain;
	  long got;
	  
	  drain = (char *)scheme_malloc_atomic(4096);

	  while ((got = scheme_get_string(who, port, drain, 0, 4096, 0, 0, 0)) != EOF) {
	    if (discard_oport)
	      scheme_put_string(who, discard_oport, drain, 0, got, 0);
	  }
	}
	return 0;
      }
    } else
      return regtry(prog, string, stringpos, stringlen, startp, endp, 0, 1);
  }

  /* Messy cases:  unanchored match. */
  spos = stringpos;
  if (!port && (prog->regstart != '\0')) {
    /* We know what char the string must start with. */
    while ((spos = l_strchr(string, spos, stringlen - (spos - stringpos), prog->regstart)) != -1) {
      if (regtry(prog, string, spos, stringlen - (spos - stringpos), startp, endp, 0, spos == stringpos))
	return 1;
      spos++;
    }
  } else {
    /* We don't know the starting char, or we have a port -- general
       case. */
    if (port) {
      rxpos len = 0, skip = 0, space = 0;
      *stringp = NULL;

      do {
	if (skip >= REGPORT_FLUSH_THRESHOLD) {
	  if (!peek) {
	    if (discard_oport)
	      scheme_put_string(who, discard_oport, *stringp, 0, skip, 0);
	    
	    scheme_get_string(who, port, *stringp, 0, skip, 0, 0, 0);

	    if (portend)
	      portend = scheme_bin_minus(portend, scheme_make_integer(skip));
	  } else {
	    peekskip = scheme_bin_plus(peekskip, scheme_make_integer(skip));
	  }

	  dropped = scheme_bin_plus(dropped, scheme_make_integer(skip));

	  len -= skip;
	  memmove(*stringp, *stringp + skip, len);
	  skip = 0;
	}

	if (regtry_port(prog, port, startp, endp, stringp, &len, &space, skip, portend, peekskip, !space)) {
	  if (!peek) {
	    char *drain;

	    if (discard_oport && *startp)
	      scheme_put_string(who, discard_oport, *stringp, 0, *startp, 0);

	    if (get_offsets)
	      drain = *stringp;
	    else
	      /* Allocate fresh in case we get different results from previous peek: */
	      drain = (char *)scheme_malloc_atomic(*endp);

	    scheme_get_string(who, port, drain, 0, *endp, 0, 0, 0);
	  }

	  *_dropped = dropped;

	  return 1;
	}
	skip++;
      } while (len >= skip);

      if (!peek) {
	/* If we get here, there must be `len' leftover characters in the port,
	   and `*stringp' must hold the characters: */
	if (len > 0) {
	  if (discard_oport)
	    scheme_put_string(who, discard_oport, *stringp, 0, len, 0);
	  scheme_get_string(who, port, *stringp, 0, len, 0, 0, 0);
	}
      }
    } else {
      rxpos e = stringpos + stringlen;
      do {
	if (regtry(prog, string, spos, stringlen - (spos - stringpos), startp, endp, 0, spos == stringpos))
	  return 1;
      } while (spos++ != e);
    }
  }

  /* Failure. */
  return 0;
}

/*
   - regtry - try match at specific point
   */
static int			/* 0 failure, 1 success */
regtry(regexp *prog, char *string, int stringpos, int stringlen, rxpos *startp, rxpos *endp, Regwork *rw, int atstart)
{
  int i;
  GC_CAN_IGNORE rxpos *sp, *ep;
  Regwork _rw;

  if (!rw) {
    rw = &_rw;
    rw->port = NULL;
  }
  rw->instr = string;
  rw->input = stringpos;
  rw->input_end = stringpos + stringlen;
  rw->startp = startp;
  rw->endp = endp;
  if (atstart)
    rw->bol = stringpos;
  else
    rw->bol = -1;

  /* ASSUMING NO GC in this loop: */ 
  sp = startp;
  ep = endp;
  for (i = prog->nsubexp; i > 0; i--) {
    *sp++ = -1;
    *ep++ = -1;
  }
  sp = ep = NULL;

#ifdef INDIRECT_TO_PROGRAM
  regstr = prog->program;
#else
  regstr = (char *)prog;
#endif
  if (regmatch(rw, N_ITO_DELTA(prog->program, 1, (char *)prog))) {
    startp[0] = stringpos;
    endp[0] = rw->input;
    return 1;
  } else
    return 0;
}

/*
   - regtry - try match in a port
   */
static int			/* 0 failure, 1 success */
regtry_port(regexp *prog, Scheme_Object *port, rxpos *startp, rxpos *endp, 
	    char **work_string, rxpos *len, rxpos *size, rxpos skip, 
	    Scheme_Object *maxlen, Scheme_Object *peekskip, 
	    int atstart)
{
  int m;
  Regwork rw;

  rw.port = port;
  rw.instr_size = *size;
  if (maxlen && SCHEME_INTP(maxlen))
    rw.input_maxend = SCHEME_INT_VAL(maxlen);
  else
    rw.input_maxend = BIGGEST_RXPOS;
  rw.peekskip = peekskip;

  m = regtry(prog, *work_string, skip, *len - skip, startp, endp, &rw, atstart);

  *work_string = rw.instr;
  *len = rw.input_end;
  *size = rw.instr_size;

  return m;
}

#define NEED_INPUT(rw, v, n) if (rw->port && (((v) + (n)) > rw->input_end)) read_more_from_regport(rw, (v) + (n))

static void read_more_from_regport(Regwork *rw, rxpos need_total)
     /* Called when we're about to look past our read-ahead */
{
  long got;
  Scheme_Object *peekskip;

  /* limit reading by rw->input_maxend: */
  if (need_total > rw->input_maxend) {
    need_total = rw->input_maxend;
    if (need_total <= rw->input_end) {
      rw->port = NULL; /* turn off further port reading */
      return;
    }
  }

  if (rw->instr_size < need_total) {
    char *naya;
    long size = rw->instr_size;
    
    size = size * 2;
    if (size < need_total)
      size += need_total;
    else if (size < 256)
      size = 256;

    naya = (char *)scheme_malloc_atomic(size);
    memcpy(naya, rw->instr, rw->input_end);

    rw->instr = naya;
    rw->instr_size = size;
  }

  rw->str = regstr; /* get_string can swap threads */

  if (rw->input_maxend < rw->instr_size)
    got = rw->input_maxend - rw->input_end;
  else
    got = rw->instr_size - rw->input_end;
  
  if (rw->peekskip)
    peekskip = scheme_bin_plus(scheme_make_integer(rw->input_end), rw->peekskip);
  else
    peekskip = scheme_make_integer(rw->input_end);

  /* Fill as much of our buffer as possible: */
  got = scheme_get_string("regexp-match", rw->port, 
			  rw->instr, rw->input_end, got,
			  1, /* read at least one char, and as much as possible */
			  1, peekskip);

  regstr = rw->str;

  if (got == EOF)
    rw->port = NULL; /* turn off further port reading */
  else {
    rw->input_end += got;

    /* Non-blocking read got enough? If not, try again in blocking mode: */
    if (need_total > rw->input_end) {
      if (rw->peekskip)
	peekskip = scheme_bin_plus(scheme_make_integer(rw->input_end), rw->peekskip);
      else
	peekskip = scheme_make_integer(rw->input_end);

      rw->str = regstr; /* get_string can swap threads */
      got = scheme_get_string("regexp-match", rw->port, 
			      rw->instr, rw->input_end, need_total - rw->input_end,
			      0, /* blocking mode */
			      1, peekskip);
      regstr = rw->str;
      
      if (got == EOF)
	rw->port = NULL; /* turn off further port reading */
      else
	rw->input_end += got;
    }
  }
}

#ifdef DO_STACK_CHECK

static Scheme_Object *regmatch_k(void)
{
  Scheme_Thread *p = scheme_current_thread;
  Regwork *rw = (Regwork *)p->ku.k.p1;
  int res;

  p->ku.k.p1 = NULL;

  regstr = rw->str; /* in case of thread swap */
 
  res = regmatch(rw, p->ku.k.i1);

  return (res ? scheme_true : scheme_false);
}

#endif

/*
   - regmatch - main matching routine
   *
   * Conceptually the strategy is simple:  check to see whether the current
   * node matches, call self recursively to see whether the rest matches,
   * and then act accordingly.  In practice we make some effort to avoid
   * recursion, in particular by going through "ordinary" nodes (that don't
   * need to know whether the rest of the match failed) by a loop instead of
   * by recursion.
   */
static int			/* 0 failure, 1 success */
regmatch(Regwork *rw, rxpos prog)
{
  rxpos scan;		/* Current node. */
  rxpos next;			/* Next node. */

#ifdef DO_STACK_CHECK
  {
# include "mzstkchk.h"
    {
      Scheme_Thread *p = scheme_current_thread;
      Regwork *rw2;
      Scheme_Object *res;

      /* rw is likely be stack allocated, so copy out to
	 the heap and then copy result back in on return. */
      rw2 = MALLOC_ONE_RT(Regwork);
      memcpy(rw2, rw, sizeof(Regwork));
#ifdef MZTAG_REQUIRED
      rw2->type = scheme_rt_regwork;
#endif

      rw2->str = regstr; /* in case of thread swap */
      p->ku.k.p1 = rw2;
      p->ku.k.i1 = prog;
      res = scheme_handle_stack_overflow(regmatch_k);

      memcpy(rw, rw2, sizeof(Regwork));

      return SCHEME_TRUEP(res);
    }
  }
#endif

  scan = prog;
  while (scan != 0) {
    next = regnext(scan);

    switch (OP(scan)) {
    case BOL:
      if (rw->input != rw->bol)
	return(0);
      break;
    case EOL:
      NEED_INPUT(rw, rw->input, 1);
      if (rw->input != rw->input_end)
	return(0);
      break;
    case ANY:
      NEED_INPUT(rw, rw->input, 1);
      if (rw->input == rw->input_end)
	return(0);
      rw->input++;
      break;
    case EXACTLY: {
      int len, i;
      rxpos opnd;

      opnd = OPSTR(OPERAND(scan));
      len = OPLEN(OPERAND(scan));
      if (rw->port) {
	/* Like the other branch, but demand chars one at a time, as
           we need them */
	for (i = 0; i < len; i++) {
	  NEED_INPUT(rw, rw->input + i, 1);
	  if (rw->input + i >= rw->input_end)
	    return 0;
	  if (regstr[opnd+i] != rw->instr[rw->input+i])
	    return 0;
	}
      } else {
	if (len > rw->input_end - rw->input)
	  return 0;
	for (i = 0; i < len; i++) {
	  if (regstr[opnd+i] != rw->instr[rw->input+i])
	    return 0;
	}
      }
      rw->input += len;
    }
      break;
    case ANYOF:
      NEED_INPUT(rw, rw->input, 1);
      if (rw->input == rw->input_end || (l_strchr(regstr, OPSTR(OPERAND(scan)), 
						  OPLEN(OPERAND(scan)), 
						  rw->instr[rw->input]) == -1))
	return(0);
      rw->input++;
      break;
    case ANYBUT:
      NEED_INPUT(rw, rw->input, 1);
      if (rw->input == rw->input_end || (l_strchr(regstr, OPSTR(OPERAND(scan)), 
						  OPLEN(OPERAND(scan)), 
						  rw->instr[rw->input]) != -1))
	return(0);
      rw->input++;
      break;
    case NOTHING:
      break;
    case BACK:
      break;
    case BRANCH: {
      rxpos save;

      if (OP(next) != BRANCH)	/* No choice. */
	next = OPERAND(scan);	/* Avoid recursion. */
      else {
	do {
	  save = rw->input;
	  if (regmatch(rw, OPERAND(scan)))
	    return(1);
	  rw->input = save;
	  scan = regnext(scan);
	} while (scan != 0 && OP(scan) == BRANCH);
	return(0);
	/* NOTREACHED */
      }
    }
      break;
    case STAR:
    case PLUS:
    case STAR2:
    case PLUS2: {
      char nextch;
      int no;
      rxpos save;
      int min;
      int nongreedy = (OP(scan) == STAR2 || OP(scan) == PLUS2);

      /*
       * Lookahead to avoid useless match attempts
       * when we know what character comes next.
       */
      nextch = '\0';
      if (OP(next) == EXACTLY)
	nextch = regstr[OPSTR(OPERAND(next))];
      min = ((OP(scan) == STAR) || (OP(scan) == STAR2)) ? 0 : 1;
      save = rw->input;

      if (nongreedy && rw->port) {
	/* Get at least one, but then don't
	   let regrepeat pull in arbitrary code: */
	Scheme_Object *saveport;
	NEED_INPUT(rw, save, 1);
	saveport = rw->port;
	rw->port = NULL;
	no = regrepeat(rw, OPERAND(scan));
	rw->port = saveport;
	nongreedy = 2;
      } else
	no = regrepeat(rw, OPERAND(scan));
      
      if (!nongreedy) {
	while (no >= min) {
	  /* If it could work, try it. */
	  if (nextch == '\0' || rw->instr[rw->input] == nextch)
	    if (regmatch(rw, next))
	      return(1);
	  /* Couldn't or didn't -- back up. */
	  no--;
	  rw->input = save + no;
	}
      } else {
	int i;
	for (i = min; i <= no; i++) {
	  rw->input = save + i;
	  /* If it could work, try it. */
	  NEED_INPUT(rw, save + i, 1);
	  if (nextch == '\0' || rw->instr[rw->input] == nextch)
	    if (regmatch(rw, next)) {
	      return(1);
	    }

	  if ((i == no) && (nongreedy == 2)) {
	    /* Maybe regrepeat can match more if we let it read from
	       the port. */
	    if ((rw->input_end - save) > no) {
	      /* We have pulled-in chars to try. */
	      int moreno;
	      Scheme_Object *saveport;

	      saveport = rw->port;
	      rw->port = NULL;
	      rw->input = save + no;
	      moreno = regrepeat(rw, OPERAND(scan));
	      rw->port = saveport;

	      if (!moreno)
		nongreedy = 1;
	      else
		no += moreno;
	    }
	  }
	}
      }
      return(0);
    }
      break;
    case END:
      return(1);		/* Success! */
      break;
    default:
      {
	int isopen;
	int no;
	rxpos save;

	switch (OP(scan)) {
	case OPENN:
	  isopen = 1;
	  no = OPLEN(OPERAND(scan));
	  if (!no)
	    no = -1; /* => don't set in result array */
	  break;
	case CLOSEN:
	  isopen = 0;
	  no = OPLEN(OPERAND(scan));
	  break;
	default:
	  if (OP(scan) < CLOSE) {
	    isopen = 1;
	    no = OP(scan) - OPEN;
	  } else {
	    isopen = 0;
	    no = OP(scan) - CLOSE;
	  }
	}

	save = rw->input;

	if (isopen) {
	  if (regmatch(rw, next)) {
	    if (no >= 0) {
	      /*
	       * Don't set startp if some later
	       * invocation of the same parentheses
	       * already has.
	       */
	      if (rw->startp[no] == -1)
		rw->startp[no] = save;
	    }
	    return(1);
	  } else
	    return(0);
	} else {
	  if (regmatch(rw, next)) {
	    if (no >= 0) {
	      /*
	       * Don't set endp if some later
	       * invocation of the same parentheses
	       * already has.
	       */
	      if (rw->endp[no] == -1)
		rw->endp[no] = save;
	    }
	    return(1);
	  } else
	    return(0);
	}
      }
      break;
    }

    scan = next;
  }

  /*
   * We get here only if there's trouble -- normally "case END" is
   * the terminating point.
   */
  regerror("corrupted pointers");
  return(0);
}

/*
   - regrepeat - repeatedly match something simple, report how many
   */
static int
regrepeat(Regwork *rw, rxpos p)
{
  int count = 0;
  rxpos scan;
  rxpos opnd;

  scan = rw->input;
  opnd = OPERAND(p);
  switch (OP(p)) {
  case ANY:
    /* need all port input: */
    while (rw->port) {
      read_more_from_regport(rw, rw->input_end + 4096);
    }
    count = rw->input_end - scan;
    scan += count;
    break;
  case EXACTLY:
    {
      rxpos opnd2 = OPSTR(opnd);
      NEED_INPUT(rw, scan, 1);
      while (regstr[opnd2] == rw->instr[scan]) {
	count++;
	scan++;
	NEED_INPUT(rw, scan, 1);
      }
    }
    break;
  case ANYOF:
    NEED_INPUT(rw, scan, 1);
    while (scan != rw->input_end 
	   && (l_strchr(regstr, OPSTR(opnd), OPLEN(opnd), rw->instr[scan]) != -1)) {
      count++;
      scan++;
      NEED_INPUT(rw, scan, 1);
    }
    break;
  case ANYBUT:
    NEED_INPUT(rw, scan, 1);
    while (scan != rw->input_end 
	   && (l_strchr(regstr, OPSTR(opnd), OPLEN(opnd), rw->instr[scan]) == -1)) {
      count++;
      scan++;
      NEED_INPUT(rw, scan, 1);
    }
    break;
  default:			/* Oh dear.  Called inappropriately. */
    regerror("internal foulup");
    count = 0;			/* Best compromise. */
    break;
  }
  rw->input = scan;

  return(count);
}

/*
   - regnext - dig the "next" pointer out of a node
   */
static rxpos
regnext(rxpos p)
{
  int offset;

  if (regstr == REGDUMMY)
    return 0;

  offset = NEXT(p);
  if (offset == 0)
    return 0;

  if (OP(p) == BACK)
    return (p-offset);
  else
    return (p+offset);
}

/*
 * strcspn - find length of initial segment of s1 consisting entirely
 * of characters not from s2
 */

static int
regstrcspn(char *s1, char *e1, char *s2)
{
  char *scan1;
  char *scan2;
  int count;

  count = 0;
  for (scan1 = s1; scan1 != e1; scan1++) {
    for (scan2 = s2; *scan2 != '\0';) { /* ++ moved down. */
      if (*scan1 == *scan2++)
	return(count);
    }
    count++;
  }
  return(count);
}

#ifndef strncpy
  extern char *strncpy();
#endif

/*
   - regsub - perform substitutions after a regexp match
   */
static 
char *regsub(regexp *prog, char *src, int sourcelen, long *lenout, char *insrc, rxpos *startp, rxpos *endp)
{
  char *dest;
  char c;
  long no;
  long len;
  long destalloc, destlen, srcpos;
	
  destalloc = 2 * sourcelen;
  destlen = 0;
  dest = (char *)scheme_malloc_atomic(destalloc + 1);
  
  srcpos = 0;
  while (srcpos < sourcelen) {
    c = src[srcpos++];
    if (c == '&')
      no = 0;
    else if (c == '\\') {
      if (src[srcpos] == '\\' || src[srcpos] == '&')
	no = -1;
      else if (src[srcpos] == '$') {
	no = prog->nsubexp + 1; /* Gives the empty string */
	srcpos++;
      } else {
	no = 0;
	while ('0' <= src[srcpos] && src[srcpos] <= '9') {
	  no = (no * 10) + (src[srcpos++] - '0');
	}
      }
    } else
      no = -1;


    if (no < 0) {		/* Ordinary character. */
      if (c == '\\' && (src[srcpos] == '\\' || src[srcpos] == '&'))
	c = src[srcpos++];
      if (destlen + 1 >= destalloc) {
	char *old = dest;
	destalloc *= 2;
	dest = (char *)scheme_malloc_atomic(destalloc + 1);
	memcpy(dest, old, destlen);
      }
      dest[destlen++] = c;
    } else if (no >= prog->nsubexp) {
      /* Number too big; prentend it's the empty string */
    } else if (startp[no] != -1 && endp[no] != -1) {
      len = endp[no] - startp[no];
      if (len + destlen >= destalloc) {
	char *old = dest;
	destalloc = 2 * destalloc + len + destlen;
	dest = (char *)scheme_malloc_atomic(destalloc + 1);
	memcpy(dest, old, destlen);
      }
      memcpy(dest + destlen, insrc + startp[no], len);
      destlen += len;
    }
  }
  dest[destlen] = '\0';

  if (lenout)
    *lenout = destlen;

  return dest;
}

static Scheme_Object *make_regexp(int argc, Scheme_Object *argv[])
{
  Scheme_Object *re;

  if (!SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("string->regexp", "string", 0, argc, argv);

  re = (Scheme_Object *)regcomp(SCHEME_STR_VAL(argv[0]), 0, SCHEME_STRTAG_VAL(argv[0]));

  if (SCHEME_IMMUTABLE_STRINGP(argv[0]))
    ((regexp *)re)->source = argv[0];
  else {
    Scheme_Object *src;
    src = scheme_make_immutable_sized_string(SCHEME_STR_VAL(argv[0]), 
					     SCHEME_STRTAG_VAL(argv[0]), 
					     1);
    ((regexp *)re)->source = src;
  }
  
  return re;
}

Scheme_Object *scheme_make_regexp(Scheme_Object *str, int * volatile result_is_err_string)
{
  volatile mz_jmp_buf save;
  Scheme_Object * volatile result;

  *result_is_err_string = 0;

  /* we rely on single-threaded, non-blocking regexp compilation: */
  memcpy((void *)&save, (void *)&scheme_error_buf, sizeof(mz_jmp_buf));
  failure_msg_for_read = "yes";
  if (!scheme_setjmp(scheme_error_buf)) {
    result = make_regexp(1, &str);
  } else {
    result = (Scheme_Object *)failure_msg_for_read;
    *result_is_err_string = 1;
  }

  failure_msg_for_read = NULL;
  memcpy((void *)&scheme_error_buf, (void *)&save, sizeof(mz_jmp_buf));
  return result;
}

static Scheme_Object *gen_compare(char *name, int pos, 
				  int argc, Scheme_Object *argv[],
				  int peek)
{
  regexp *r;
  char *full_s;
  rxpos *startp, *endp;
  int offset = 0, endset, m;
  Scheme_Object *iport, *oport = NULL, *startv = NULL, *endv = NULL, *dropped;
  
  if (SCHEME_TYPE(argv[0]) != scheme_regexp_type
      && !SCHEME_STRINGP(argv[0]))
    scheme_wrong_type(name, "regexp-or-string", 0, argc, argv);
  if ((peek || !SCHEME_STRINGP(argv[1]))
      && !SCHEME_INPORTP(argv[1]))
    scheme_wrong_type(name, peek ? "input-port" : "string or input-port", 1, argc, argv);
  
  if (SCHEME_INPORTP(argv[1])) {
    iport = argv[1];
    endset = -2;
  } else {
    iport = NULL;
    endset = SCHEME_STRLEN_VAL(argv[1]);
  }

  if (argc > 2) {
    int len = endset;

    offset = scheme_extract_index(name, 2, argc, argv, len + 1, 0);

    if (!iport && (offset > len)) {
      scheme_out_of_string_range(name, "offset ", argv[2], argv[1], 0, len);
      return NULL;
    } else if (offset < 0) {
      /* argument was a bignum */
      offset = 0x7FFFFFFF;
    }
    startv = argv[2];
      

    if (argc > 3) {
      if (!SCHEME_FALSEP(argv[3])) {
	endset = scheme_extract_index(name, 3, argc, argv, len + 1, 1);

	if (iport) {
	  if (endset < 0) {
	    /* argument was a bignum */
	    endset = 0x7FFFFFFF;
	  }
	  /* compare numbers */
	  if (scheme_bin_lt(argv[3], argv[2])) {
	    scheme_raise_exn(MZEXN_APPLICATION_MISMATCH,
			     argv[3],
			     "%s: ending index %V is smaller than starting index %V for port",
			     name, argv[3], argv[2]);
	    return NULL;
	  }
	} else if (endset < offset || endset > len) {
	  scheme_out_of_string_range(name, "ending ", argv[3], argv[1], offset, len);
	  return NULL;
	}
	endv = argv[3];
      }

      if (argc > 4) {
	if (SCHEME_TRUEP(argv[4])) {
	  if (!SCHEME_OUTPORTP(argv[4]))
	    scheme_wrong_type(name, "output-port or #f", 4, argc, argv);
	  oport = argv[4];
	}
      }
    }
  }

  if (iport && !startv)
    startv = scheme_make_integer(0);

  if (SCHEME_STRINGP(argv[0]))
    r = regcomp(SCHEME_STR_VAL(argv[0]), 0, SCHEME_STRTAG_VAL(argv[0]));
  else
    r = (regexp *)argv[0];

  if (!iport)
    full_s = SCHEME_STR_VAL(argv[1]);
  else
    full_s = NULL;

  startp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);
  endp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);

  dropped = scheme_make_integer(0);

  m = regexec(name, r, full_s, offset, endset - offset, startp, endp,
	      iport, &full_s, peek, pos, oport, startv, endv, &dropped);

  if (m) {
    int i;
    Scheme_Object *l = scheme_null;

    if (oport && !iport)
      scheme_put_string(name, oport, full_s, 0, *startp, 0);

    for (i = r->nsubexp; i--; ) {
      if (startp[i] != -1) {
	if (pos) {
	  Scheme_Object *startpd, *endpd;

	  startpd = scheme_make_integer(startp[i]);
	  endpd = scheme_make_integer(endp[i]);
	  
	  if (iport) {
	    /* Increment by drop count: */
	    startpd = scheme_bin_plus(startpd, dropped);
	    endpd = scheme_bin_plus(endpd, dropped);
	  }
	  
	  l = scheme_make_pair(scheme_make_pair(startpd, endpd),
			       l);
	} else {
	  long len;
	  len = endp[i] - startp[i];
	  l = scheme_make_pair(scheme_make_sized_offset_string(full_s, startp[i], 
							       len,
							       1),
			       l);
	}
      } else
	l = scheme_make_pair(scheme_false, l);
    }

    return l;
  } else {
    if (oport && !iport)
      scheme_put_string(name, oport, full_s, 0, endset, 0);

    return scheme_false;
  }
}

static Scheme_Object *compare(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match", 0, argc, argv, 0);
}

static Scheme_Object *positions(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-positions", 1, argc, argv, 0);
}

static Scheme_Object *compare_peek(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek", 0, argc, argv, 1);
}

static Scheme_Object *positions_peek(int argc, Scheme_Object *argv[])
{
  return gen_compare("regexp-match-peek-positions", 1, argc, argv, 1);
}

static Scheme_Object *gen_replace(int argc, Scheme_Object *argv[], int all)
{
  regexp *r;
  char *source, *prefix = NULL;
  rxpos *startp, *endp;
  int prefix_len = 0, sourcelen, srcoffset = 0;

  if (SCHEME_TYPE(argv[0]) != scheme_regexp_type
      && !SCHEME_STRINGP(argv[0]))
    scheme_wrong_type("regexp-replace", "regexp-or-string", 0, argc, argv);
  if (!SCHEME_STRINGP(argv[1]))
    scheme_wrong_type("regexp-replace", "string", 1, argc, argv);
  if (!SCHEME_STRINGP(argv[2]))
    scheme_wrong_type("regexp-replace", "string", 2, argc, argv);

  if (SCHEME_STRINGP(argv[0])) {
    r = regcomp(SCHEME_STR_VAL(argv[0]), 0, SCHEME_STRTAG_VAL(argv[0]));
  } else
    r = (regexp *)argv[0];

  source = SCHEME_STR_VAL(argv[1]);
  sourcelen = SCHEME_STRTAG_VAL(argv[1]);

  startp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);
  endp = MALLOC_N_ATOMIC(rxpos, r->nsubexp);

  while (1) {
    int m;

    m = regexec("regexp-replace", r, source, srcoffset, sourcelen - srcoffset, startp, endp,
		NULL, NULL, 0, 0, NULL, NULL, NULL, NULL);

    if (m) {
      char *insert;
      long len, end, startpd, endpd;
      
      insert = regsub(r, SCHEME_STR_VAL(argv[2]), SCHEME_STRTAG_VAL(argv[2]), &len, 
		      source, startp, endp);
      
      end = SCHEME_STRTAG_VAL(argv[1]);
      
      startpd = startp[0];
      endpd = endp[0];
      
      if (!startpd && (endpd == end) && !prefix)
	return scheme_make_sized_string(insert, len, 0);
      else if (!all) {
	char *result;
	long total;
	
	total = len + (startpd - srcoffset) + (end - endpd);
	
	result = (char *)scheme_malloc_atomic(total + 1);
	memcpy(result, source + srcoffset, startpd - srcoffset);
	memcpy(result + (startpd - srcoffset), insert, len);
	memcpy(result + (startpd - srcoffset) + len, source + endpd, (end - endpd) + 1);
	
	return scheme_make_sized_string(result, total, 0);
      } else {
	char *naya;
	long total;
	
	total = len + prefix_len + (startpd - srcoffset);
	
	naya = (char *)scheme_malloc_atomic(total + 1);
	memcpy(naya, prefix, prefix_len);
	memcpy(naya + prefix_len, source + srcoffset, startpd - srcoffset);
	memcpy(naya + prefix_len + (startpd - srcoffset), insert, len);

	prefix = naya;
	prefix_len = total;

	srcoffset = endpd;
      }
    } else if (!prefix)
      return argv[1];
    else {
      char *result;
      long total, slen;
      
      slen = sourcelen - srcoffset;
      total = prefix_len + slen;
      
      result = (char *)scheme_malloc_atomic(total + 1);
      memcpy(result, prefix, prefix_len);
      memcpy(result + prefix_len, source + srcoffset, slen);
      result[prefix_len + slen] = 0;
      
      return scheme_make_sized_string(result, total, 0);
    }

    SCHEME_USE_FUEL(1);
  }
}

static Scheme_Object *replace(int argc, Scheme_Object *argv[])
{
  return gen_replace(argc, argv, 0);
}

static Scheme_Object *replace_star(int argc, Scheme_Object *argv[])
{
  return gen_replace(argc, argv, 1);
}

static Scheme_Object *regexp_p(int argc, Scheme_Object *argv[])
{
  return (SCHEME_TYPE(argv[0]) == scheme_regexp_type) ? scheme_true : scheme_false;
}

Scheme_Object *scheme_regexp_source(Scheme_Object *re)
{
  return ((regexp *)re)->source;
}

#ifdef MZ_PRECISE_GC
START_XFORM_SKIP;
#define MARKS_FOR_REGEXP_C
#include "mzmark.c"
END_XFORM_SKIP;
#endif

void scheme_regexp_initialize(Scheme_Env *env)
{
#ifdef MZ_PRECISE_GC
  GC_REG_TRAV(scheme_regexp_type, mark_regexp);
  GC_REG_TRAV(scheme_rt_regwork, mark_regwork);
#endif

  REGISTER_SO(regparsestr);
  REGISTER_SO(regstr);

  scheme_add_global_constant("regexp", 
			     scheme_make_prim_w_arity(make_regexp, 
						      "regexp", 
						      1, 1), 
			     env);
  scheme_add_global_constant("regexp-match",
			     scheme_make_prim_w_arity(compare,
						      "regexp-match",
						      2, 5),
			     env);
  scheme_add_global_constant("regexp-match-positions", 
			     scheme_make_prim_w_arity(positions, 
						      "regexp-match-positions", 
						      2, 5),
			     env);
  scheme_add_global_constant("regexp-match-peek",
			     scheme_make_prim_w_arity(compare_peek,
						      "regexp-match-peek",
						      2, 4),
			     env);
  scheme_add_global_constant("regexp-match-peek-positions", 
			     scheme_make_prim_w_arity(positions_peek, 
						      "regexp-match-peek-positions",
						      2, 4),
			     env);
  scheme_add_global_constant("regexp-replace", 
			     scheme_make_prim_w_arity(replace, 
						      "regexp-replace", 
						      3, 3), 
			     env);
  scheme_add_global_constant("regexp-replace*", 
			     scheme_make_prim_w_arity(replace_star, 
						      "regexp-replace*", 
						      3, 3), 
			     env);
  scheme_add_global_constant("regexp?", 
			     scheme_make_prim_w_arity(regexp_p, 
						      "regexp?", 
						      1, 1), 
			     env);
}

#endif
/* NO_REGEXP_UTILS */
