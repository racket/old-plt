// This file was adapted to usage in non-GNU environments
// (especially under wxWindows) by Stefan Hammes (steve),
// (stefan.hammes@urz.uni-heidelberg.de)
//
// The contents of this file are the combined headerfiles
// for the GNU C++ Regex class and the GNU C regex code.
//
// To use this code indepently from libg++ all classes
// were renamed by adding a 'wx' in front of their names!
//
// 01.12.94


// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/


#ifndef _wxRegex_h
#ifdef __GNUG__
#pragma interface
#endif
#define _wxRegex_h 1

#if defined(SHORT_NAMES) || defined(VMS)
#define wxRE_compile_pattern	recmppat
#define wxRE_pattern_buffer	repatbuf
#define wxRE_registers		reregs
#endif

struct wxRE_pattern_buffer;       // defined elsewhere
struct wxRE_registers;

class wxRegex
{
private:

                     wxRegex(const wxRegex&) {}  // no X(X&)
  void               operator = (const wxRegex&) {} // no assignment

protected:
  wxRE_pattern_buffer* buf;
  wxRE_registers*      reg;

public:
                     wxRegex(const char* t, 
                           int fast = 0, 
                           int bufsize = 40, 
                           const char* transtable = 0);

                    ~wxRegex();

  int                Match(const char* s, int len, int pos = 0) const;
  int                Search(const char* s, int len, 
                            int& matchlen, int startpos = 0) const;
  int                match_info(int& start, int& length, int nth = 0) const;

  int                OK() const;  // representation invariant
};

// some built in regular expressions

extern const wxRegex RXwhite;          // = "[ \n\t\r\v\f]+"
extern const wxRegex RXint;            // = "-?[0-9]+"
extern const wxRegex RXdouble;         // = "-?\\(\\([0-9]+\\.[0-9]*\\)\\|
                                     //    \\([0-9]+\\)\\|\\(\\.[0-9]+\\)\\)
                                     //    \\([eE][---+]?[0-9]+\\)?"
extern const wxRegex RXalpha;          // = "[A-Za-z]+"
extern const wxRegex RXlowercase;      // = "[a-z]+"
extern const wxRegex RXuppercase;      // = "[A-Z]+"
extern const wxRegex RXalphanum;       // = "[0-9A-Za-z]+"
extern const wxRegex RXidentifier;     // = "[A-Za-z_][A-Za-z0-9_]*"


#endif



/* Definitions for data structures callers pass the regex library.

   Copyright (C) 1985, 1989-92 Free Software Foundation, Inc.

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifndef __REGEXP_LIBRARY
#define __REGEXP_LIBRARY

#if defined(SHORT_NAMES) || defined(VMS)
#define wxRE_compile_pattern	recmppat
#define wxRE_pattern_buffer	repatbuf
#define wxRE_registers		reregs
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Define number of parens for which we record the beginnings and ends.
   This affects how much space the `struct wxRE_registers' type takes up.  */
#ifndef wxRE_NREGS
#define wxRE_NREGS 10
#endif

#define BYTEWIDTH 8


/* Maximum number of duplicates an interval can allow.  */

#ifndef wxRE_DUP_MAX /* kludge for AIX, which defines it */
#ifdef wx_msw   // defined for wxWindows under MS-windows
#define wxRE_DUP_MAX  ((1 << 14) - 1) // for the PC, changed by steve, 1.12.94
#include <malloc.h>
#if !defined(__BORLANDC__) && !defined(__WATCOMC__)
#define alloca _alloca
#endif
#else
#define wxRE_DUP_MAX  ((1 << 15) - 1) 
#endif
#endif

/* This defines the various regexp syntaxes.  */
extern int wxObscure_syntax;


/* The following bits are used in the wxObscure_syntax variable to choose among
   alternative regexp syntaxes.  */

/* If this bit is set, plain parentheses serve as grouping, and backslash
     parentheses are needed for literal searching.
   If not set, backslash-parentheses are grouping, and plain parentheses
     are for literal searching.  */
#define wxRE_NO_BK_PARENS	1

/* If this bit is set, plain | serves as the `or'-operator, and \| is a 
     literal.
   If not set, \| serves as the `or'-operator, and | is a literal.  */
#define wxRE_NO_BK_VBAR (1 << 1)

/* If this bit is not set, plain + or ? serves as an operator, and \+, \? are 
     literals.
   If set, \+, \? are operators and plain +, ? are literals.  */
#define wxRE_BK_PLUS_QM (1 << 2)

/* If this bit is set, | binds tighter than ^ or $.
   If not set, the contrary.  */
#define wxRE_TIGHT_VBAR (1 << 3)

/* If this bit is set, then treat newline as an OR operator.
   If not set, treat it as a normal character.  */
#define wxRE_NEWLINE_OR (1 << 4)

/* If this bit is set, then special characters may act as normal
   characters in some contexts. Specifically, this applies to:
	^ -- only special at the beginning, or after ( or |;
	$ -- only special at the end, or before ) or |;
	*, +, ? -- only special when not after the beginning, (, or |.
   If this bit is not set, special characters (such as *, ^, and $)
   always have their special meaning regardless of the surrounding
   context.  */
#define wxRE_CONTEXT_INDEP_OPS (1 << 5)

/* If this bit is not set, then \ before anything inside [ and ] is taken as 
     a real \.
   If set, then such a \ escapes the following character.  This is a
     special case for awk.  */
#define wxRE_AWK_CLASS_HACK (1 << 6)

/* If this bit is set, then \{ and \} or { and } serve as interval operators.
   If not set, then \{ and \} and { and } are treated as literals.  */
#define wxRE_INTERVALS (1 << 7)

/* If this bit is not set, then \{ and \} serve as interval operators and 
     { and } are literals.
   If set, then { and } serve as interval operators and \{ and \} are 
     literals.  */
#define wxRE_NO_BK_CURLY_BRACES (1 << 8)

/* If this bit is set, then character classes are supported; they are:
     [:alpha:],	[:upper:], [:lower:],  [:digit:], [:alnum:], [:xdigit:],
     [:space:], [:print:], [:punct:], [:graph:], and [:cntrl:].
   If not set, then character classes are not supported.  */
#define wxRE_CHAR_CLASSES (1 << 9)

/* If this bit is set, then the dot re doesn't match a null byte.
   If not set, it does.  */
#define wxRE_DOT_NOT_NULL (1 << 10)

/* If this bit is set, then [^...] doesn't match a newline.
   If not set, it does.  */
#define wxRE_HAT_NOT_NEWLINE (1 << 11)

/* If this bit is set, back references are recognized.
   If not set, they aren't.  */
#define wxRE_NO_BK_REFS (1 << 12)

/* If this bit is set, back references must refer to a preceding
   subexpression.  If not set, a back reference to a nonexistent
   subexpression is treated as literal characters.  */
#define wxRE_NO_EMPTY_BK_REF (1 << 13)

/* If this bit is set, bracket expressions can't be empty.  
   If it is set, they can be empty.  */
#define wxRE_NO_EMPTY_BRACKETS (1 << 14)

/* If this bit is set, then *, +, ? and { cannot be first in an re or
   immediately after a |, or a (.  Furthermore, a | cannot be first or
   last in an re, or immediately follow another | or a (.  Also, a ^
   cannot appear in a nonleading position and a $ cannot appear in a
   nontrailing position (outside of bracket expressions, that is).  */
#define wxRE_CONTEXTUAL_INVALID_OPS (1 << 15)

/* If this bit is set, then +, ? and | aren't recognized as operators.
   If it's not, they are.  */
#define wxRE_LIMITED_OPS (1 << 16)

/* If this bit is set, then an ending range point has to collate higher
     or equal to the starting range point.
   If it's not set, then when the ending range point collates higher
     than the starting range point, the range is just considered empty.  */
#define wxRE_NO_EMPTY_RANGES (1 << 17)

/* If this bit is set, then a hyphen (-) can't be an ending range point.
   If it isn't, then it can.  */
#define wxRE_NO_HYPHEN_RANGE_END (1 << 18)


/* Define combinations of bits for the standard possibilities.  */
#define wxRE_SYNTAX_POSIX_AWK (wxRE_NO_BK_PARENS | wxRE_NO_BK_VBAR \
			| wxRE_CONTEXT_INDEP_OPS)
#define wxRE_SYNTAX_AWK (wxRE_NO_BK_PARENS | wxRE_NO_BK_VBAR \
			| wxRE_CONTEXT_INDEP_OPS | wxRE_AWK_CLASS_HACK)
#define wxRE_SYNTAX_EGREP (wxRE_NO_BK_PARENS | wxRE_NO_BK_VBAR \
			| wxRE_CONTEXT_INDEP_OPS | wxRE_NEWLINE_OR)
#define wxRE_SYNTAX_GREP (wxRE_BK_PLUS_QM | wxRE_NEWLINE_OR)
#define wxRE_SYNTAX_EMACS 0
#define wxRE_SYNTAX_POSIX_BASIC (wxRE_INTERVALS | wxRE_BK_PLUS_QM 		\
			| wxRE_CHAR_CLASSES | wxRE_DOT_NOT_NULL 		\
                        | wxRE_HAT_NOT_NEWLINE | wxRE_NO_EMPTY_BK_REF 	\
                        | wxRE_NO_EMPTY_BRACKETS | wxRE_LIMITED_OPS		\
                        | wxRE_NO_EMPTY_RANGES | wxRE_NO_HYPHEN_RANGE_END)	
                        
#define wxRE_SYNTAX_POSIX_EXTENDED (wxRE_INTERVALS | wxRE_NO_BK_CURLY_BRACES	   \
			| wxRE_NO_BK_VBAR | wxRE_NO_BK_PARENS 		   \
                        | wxRE_HAT_NOT_NEWLINE | wxRE_CHAR_CLASSES 		   \
                        | wxRE_NO_EMPTY_BRACKETS | wxRE_CONTEXTUAL_INVALID_OPS \
                        | wxRE_NO_BK_REFS | wxRE_NO_EMPTY_RANGES 		   \
                        | wxRE_NO_HYPHEN_RANGE_END)


/* This data structure is used to represent a compiled pattern.  */

struct wxRE_pattern_buffer
  {
    char *buffer;	/* Space holding the compiled pattern commands.  */
    long allocated;	/* Size of space that `buffer' points to. */
    long used;		/* Length of portion of buffer actually occupied  */
    char *fastmap;	/* Pointer to fastmap, if any, or zero if none.  */
			/* wxRE_search uses the fastmap, if there is one,
			   to skip over totally implausible characters.  */
    char *translate;	/* Translate table to apply to all characters before 
		           comparing, or zero for no translation.
			   The translation is applied to a pattern when it is 
                           compiled and to data when it is matched.  */
    char fastmap_accurate;
			/* Set to zero when a new pattern is stored,
			   set to one when the fastmap is updated from it.  */
    char can_be_null;   /* Set to one by compiling fastmap
			   if this pattern might match the null string.
			   It does not necessarily match the null string
			   in that case, but if this is zero, it cannot.
			   2 as value means can match null string
			   but at end of range or before a character
			   listed in the fastmap.  */
  };


/* search.c (search_buffer) needs this one value.  It is defined both in
   regex.c and here.  */
#define wxRE_EXACTN_VALUE 1


/* Structure to store register contents data in.

   Pass the address of such a structure as an argument to wxRE_match, etc.,
   if you want this information back.

   For i from 1 to wxRE_NREGS - 1, start[i] records the starting index in
   the string of where the ith subexpression matched, and end[i] records
   one after the ending index.  start[0] and end[0] are analogous, for
   the entire pattern.  */

struct wxRE_registers
  {
    int start[wxRE_NREGS];
    int end[wxRE_NREGS];
  };



#if defined(__STDC__) || defined(__cplusplus)

extern char *wxRE_compile_pattern (const char *, int, struct wxRE_pattern_buffer *);
/* Is this really advertised?  */
extern void wxRE_compile_fastmap (struct wxRE_pattern_buffer *);
extern int wxRE_search (struct wxRE_pattern_buffer *, char*, int, int, int,
		      struct wxRE_registers *);
extern int wxRE_search_2 (struct wxRE_pattern_buffer *, char *, int,
			char *, int, int, int,
			struct wxRE_registers *, int);
extern int wxRE_match (struct wxRE_pattern_buffer *, char *, int, int,
		     struct wxRE_registers *);
extern int wxRE_match_2 (struct wxRE_pattern_buffer *, char *, int,
		       char *, int, int, struct wxRE_registers *, int);

#if 0
/* 4.2 bsd compatibility.  */
extern char *wxRE_comp (char *);
extern int wxRE_exec (char *);
#endif

#else /* !__STDC__ */

#define const /* nothing */
extern char *wxRE_compile_pattern ();
/* Is this really advertised? */
extern void wxRE_compile_fastmap ();
extern int wxRE_search (), wxRE_search_2 ();
extern int wxRE_match (), wxRE_match_2 ();

#if 0
/* 4.2 bsd compatibility.  */
extern char *wxRE_comp ();
extern int wxRE_exec ();
#endif

#endif /* __STDC__ */


#ifdef SYNTAX_TABLE
extern char *wxRE_syntax_table;
#endif

#ifdef __cplusplus
extern int wxRE_max_failures;
}
#endif

#endif /* !__REGEXP_LIBRARY */
