Readme for wxString class in 'contrib\wxstring'
-----------------------------------------------
Last update: 25.05.95

Contents
--------
1. General comments and additions by Stefan Hammes
2. GNU documentation of wxString and wxRegex classes
3. GNU documentation about regular expressions (parts of).

///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
1. General
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////

Introduction
------------
Strings are used very frequently in most programs. There is no direct support in
the C++ language for strings. Therefore a string class can be useful in many
situations. It not only makes the code shorter and easier to read, it also
provides more security, because we don't have to deal with pointer acrobatics.

In former versions of wxWindows we had a 'little' string class which was useful,
but not very elaborate and efficient. Therefore I have included a much more
powerful and very efficient string class with this contribution.

For backward compatibility I have also included most of the member functions
of the old wxString class. I have left out a few members, which were too
dangerous :-)

The new wxString class is a 'port' and extension of the GNU 'String' 
class from libg++. It can be compiled under MSW, UNIX and VMS 
(see below for compilers). 
I have capitalized the function names to be consistent with
the wxWindows naming scheme.

The reasons for not using the GNU string class directly are:
- It is not available on all systems (mostly its only available on some 
  UNIX systems).
- We can make changes and extensions to the string class as needed and are not
  forced to use 'only' the functionality of the GNU string class.


Copyright of the original GNU code portion
------------------------------------------
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

  
Features
--------
The wxString class offers many string handling functions and a support for
regular expression. With this help, pattern matching can be made much
easier.
See below for a discussion of the GNU features of wxString. I would also
like to refer you to the headerfile 'wxstrgnu.h' which shows all memberfunctions
and is somewhat selfexplanatory.


Additions/Modifications
-----------------------
As stated above, I have done some extensions to the wxString class.
This includes the including of the 'old' wxString class member functions.
In the following I list the memberfunctions which were added by me:

- Access to the internal representation. Should be used with care:
  char* GetData() const;
  
- To make a copy of 'this' (only for compatibility):
  wxString Copy() const;

- For case sensitive and case insensitive comparisons:
  enum caseCompare {exact, ignoreCase};
  int CompareTo(const char* cs,     caseCompare cmp = exact) const;
  int CompareTo(const wxString& st, caseCompare cmp = exact) const;

- For case sensitive and case insensitive containment check:
  Bool Contains(const char* pat,     caseCompare cmp = exact) const;
  Bool Contains(const wxString& pat, caseCompare cmp = exact) const;

- For case sensitive and case insensitive index calculation:
  int Index(const char* pat, int i=0,   caseCompare cmp = exact) const;
  int Index(const wxString& s, int i=0, caseCompare cmp = exact) const;
  
- For element access in addition to the [] operator:
  char& operator()(int);     // Indexing with  bounds checking

- To put something in front of a string:
  wxString& Prepend(const char*);           // Prepend a character string
  wxString& Prepend(const wxString& s);
  wxString& Prepend(char c, int rep=1);  // Prepend c rep times

- For concatenation:
  wxString& Append(const char* cs);
  wxString& Append(const wxString& s);
  wxString& Append(char c, int rep=1);   // Append c rep times

- To get the first and last occurrence of a char or string:
  int First(char c) const;
  int First(const char* cs) const;
  int First(const wxString& cs) const;
  int Last(char c) const;
  int Last(const char* cs) const;
  int Last(const wxString& cs) const;
  
- To insert something into a string              
  wxString& Insert(int pos, const char*);
  wxString& Insert(int pos, const wxString&);

- To remove data (in addition to the 'Del' functions):
  wxString& Remove(int pos);        // Remove pos to end of string
  wxString& Remove(int pos, int n); // Remove n chars starting at pos
  wxString& RemoveLast(void);       // It removes the last char of a string

- To replace data:
  wxString& Replace(int pos, int n, const char*);
  wxString& Replace(int pos, int n, const wxString&);

- Alternative names for compatibility:
  void LowerCase();              // Change self to lower-case
  void UpperCase();              // Change self to upper-case

- Edward Z.'s additions:
  wxString SubString(int from, int to);

- formatted assignment:
  void sprintf(const char *fmt, ...);

  we do not use the 'sprintf' constructor of the old wxString class anymore, 
  because with that constructor, every initialisation with a string would 
  go through sprintf and this is not desireable, because sprintf interprets
  some characters. with the above function we can write i.e.:

  wxString msg; msg.sprintf("Processing item %d\n",count);
  
- strip chars at the front and/or end:
  enum        StripType {leading = 0x1, trailing = 0x2, both = 0x3};
  wxSubString Strip(StripType s=trailing, char c=' ');
  
  This can be useful for trimming strings.
  
- Line input:  
  friend int  Readline(FILE *f, wxString& x, 
                       char terminator = '\n',
                       int discard_terminator = 1);
                             
  Besides the stream I/O functions this function can be used for non-standard
  formatted I/O with arbitrary line terminators.                            

- The GNU wxString class lacks some classification functions:
  int IsAscii() const;
  int IsWord() const;
  int IsNumber() const;
  int IsNull() const;
  int IsDefined() const;

- The meaning of nil has been changed. A wxString x is only nil, if it
  has been declared `wxString x'. In all other cases it is NOT nil. This
  seems to me more logical than to let a `wxString x=""' be nil as it
  was in the original GNU code.

- IMPORTANT!!!!!!!!!!!!!
// here is a very, very, very ugly macro, but it makes things more
// transparent in cases, where a library function requires a 
// (char *) argument. this is especially the case in wxWindows,
// where most char-arguments are (char *) and not (const char *).
// this macro should only be used in such cases and NOT to
// modify the internal data. The standard type conversion function
// of wxString returns a '(const char *)'.
// the conventional way would be 'function((char *)string.Chars())'.
// with the macro this can be achieved by 'function(wxCHARARG(string))'.
// this makes it better clear, that the usage should be confined
// to arguments! See section below for examples.

#define wxCHARARG(s) ((char *)(s).Chars())  


Function calls
--------------
When using wxString objects as parameters to other functions you should
note the following:

void f1(const char *s){}
void f2(char *s){}

main(){
  wxString aString;
  f1(aString); // ok
  f2(aString); // error
  f2(wxCHARARG(aString)); // ok
  printf("%s",aString); // NO compilation error, but a runtime error.
  printf("%s",aString.Chars()); // ok
  printf("%s",wxCHARARG(aString)); // ok
}
  

Headerfiles
-----------
For DOS and UNIX we use a stub-headerfile '\wxw161\include\base\wxstring.h'
which includes the two headerfiles in the '\wxw161\contrib\wxstring' directory.

For VMS we have to do an addition due to the bad inclusion mechanism:
In the VMS-Makefile, the include-file search path is augmented with the 
'\wxw161\contrib\wxstring' directory, so that the correct headerfiles 
can be included.

So you have only to specify '#define USE_GNU_WXSTRING 1' in
'\wxw161\include\base\wx_setup.h' to use the wxString class.


Testprogram
-----------
I have included a testprogram 'test.cc' for many features
of wxString and wxRegex. It also checks the extensions which
were made by me. When running the compiled program, there should
be NO assert-errors if everything is ok!


Compilers
---------
I have compiled the wxString and wxRegex class successfully with the following
compilers (it should work on nearly every compiler):

PC    MS-Visual C++ 1.0
UNIX  gcc v2.6.3
VMS   DEC C++ compiler

Warnings about type conversion or assignments can be ignored.

VMS
---
Some things are special for VMS compilation. I have integrated them
into all necessary files.


If you have problems, please send an E-mail to me. 
I wish you success,

Stefan Hammes (stefan.hammes@urz.uni-heidelberg.de)

/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
2. In the following I list the original GNU wxString and wxRegex
   documentation. It describes most functions of the classes.
   I have capitalized the function names to be consistent with
   the wxWindows naming scheme.
   The examples are integrated into the test program.
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

   Copyright (C) 1988, 1991, 1992 Free Software Foundation, Inc.

   Permission is granted to make and distribute verbatim copies of this
manual provided the copyright notice and this permission notice are
preserved on all copies.

   Permission is granted to copy and distribute modified versions of
this manual under the conditions for verbatim copying, provided also
that the section entitled "GNU Library General Public License" is
included exactly as in the original, and provided that the entire
resulting derived work is distributed under the terms of a permission
notice identical to this one.

   Permission is granted to copy and distribute translations of this
manual into another language, under the above conditions for modified
versions, except that the section entitled "GNU Library General Public
License" and this permission notice may be included in translations
approved by the Free Software Foundation instead of in the original
English.

The wxString class
******************

   The `wxString' class is designed to extend GNU C++ to support string
processing capabilities similar to those in languages like Awk.  The
class provides facilities that ought to be convenient and efficient
enough to be useful replacements for `char*' based processing via the C
string library (i.e., `strcpy, strcmp,' etc.) in many applications.
Many details about wxString representations are described in the
Representation section.

   A separate `wxSubString' class supports substring extraction and
modification operations. This is implemented in a way that user
programs never directly construct or represent substrings, which are
only used indirectly via wxString operations.

   Another separate class, `wxRegex' is also used indirectly via wxString
operations in support of regular expression searching, matching, and the
like.  The wxRegex class is based entirely on the GNU Emacs regex
functions.  *Note Syntax of Regular Expressions: (emacs.info)Regexps,
for a full explanation of regular expression syntax.  (For
implementation details, see the internal documentation in files
`regex.h' and `regex.c'.)

Constructors
============

   Strings are initialized and assigned as in the following examples:

`wxString x;'
     Set x to the nil string. This is different from the original GNU code
     which sets a strings also to nil when it is assign 0 or "".

`wxString x = "Hello"; wxString y("Hello");'
     Set x and y to a copy of the string "Hello".

`wxString x = 'A'; wxString y('A');'
     Set x and y to the string value "A"

`wxString u = x; wxString v(x);'
     Set u and v to the same string as wxString x

`wxString u = x.At(1,4); wxString v(x.At(1,4));'
     Set u and v to the length 4 substring of x starting at position 1
     (counting indexes from 0).

`wxString x("abc", 2);'
     Sets x to "ab", i.e., the first 2 characters of "abc".

   There are no directly accessible forms for declaring wxSubString
variables.

   The declaration `wxRegex r("[a-zA-Z_][a-zA-Z0-9_]*");' creates a
compiled regular expression suitable for use in wxString operations
described below. (In this case, one that matches any C++ identifier).
The first argument may also be a wxString.  Be careful in distinguishing
the role of backslashes in quoted GNU C++ char* constants versus those
in Regexes. For example, a wxRegex that matches either one or more tabs
or all strings beginning with "ba" and ending with any number of
occurrences of "na" could be declared as `wxRegex r =
"\\(\t+\\)\\|\\(ba\\(na\\)*\\)"' Note that only one backslash is needed
to signify the tab, but two are needed for the parenthesization and
virgule, since the GNU C++ lexical analyzer decodes and strips
backslashes before they are seen by wxRegex.

   There are three additional optional arguments to the wxRegex
constructor that are less commonly useful:

`fast (default 0)'
     `fast' may be set to true (1) if the wxRegex should be
     "fast-compiled". This causes an additional compilation step that
     is generally worthwhile if the wxRegex will be used many times.

`bufsize (default max(40, length of the string))'
     This is an estimate of the size of the internal compiled
     expression. Set it to a larger value if you know that the
     expression will require a lot of space. If you do not know, do not
     worry: realloc is used if necessary.

`transtable (default none == 0)'
     The address of a byte translation table (a char[256]) that
     translates each character before matching.

   As a convenience, several Regexes are predefined and usable in any
program. Here are their declarations from `wxString.h'.

     extern wxRegex RXwhite;      // = "[ \n\t]+"
     extern wxRegex RXint;        // = "-?[0-9]+"
     extern wxRegex RXdouble;     // = "-?\\(\\([0-9]+\\.[0-9]*\\)\\|
                                  //    \\([0-9]+\\)\\|
                                  //    \\(\\.[0-9]+\\)\\)
                                  //    \\([eE][---+]?[0-9]+\\)?"
     extern wxRegex RXalpha;      // = "[A-Za-z]+"
     extern wxRegex RXlowercase;  // = "[a-z]+"
     extern wxRegex RXuppercase;  // = "[A-Z]+"
     extern wxRegex RXalphanum;   // = "[0-9A-Za-z]+"
     extern wxRegex RXidentifier; // = "[A-Za-z_][A-Za-z0-9_]*"

Examples
========

   Most `wxString' class capabilities are best shown via example.  The
examples below use the following declarations.

         wxString x = "Hello";
         wxString y = "world";
         wxString n = "123";
         wxString z;
         char*  s = ",";
         wxString lft, mid, rgt;
         wxRegex  r = "e[a-z]*o";
         wxRegex  r2("/[a-z]*/");
         char   c;
         int    i, pos, len;
         double f;
         wxString words[10];
         words[0] = "a";
         words[1] = "b";
         words[2] = "c";

Comparing, Searching and Matching
=================================

   The usual lexicographic relational operators (`==, !=, <, <=, >, >=')
are defined. A functional form `compare(wxString, wxString)' is also
provided, as is `fcompare(wxString, wxString)', which compares Strings
without regard for upper vs. lower case.

   All other matching and searching operations are based on some form
of the (non-public) `match' and `search' functions.  `match' and
`search' differ in that `match' attempts to match only at the given
starting position, while `search' starts at the position, and then
proceeds left or right looking for a match.  As seen in the following
examples, the second optional `startpos' argument to functions using
`match' and `search' specifies the starting position of the search: If
non-negative, it results in a left-to-right search starting at position
`startpos', and if negative, a right-to-left search starting at
position `x.Length() + startpos'. In all cases, the index returned is
that of the beginning of the match, or -1 if there is no match.

   Three wxString functions serve as front ends to `search' and `match'.
`index' performs a search, returning the index, `matches' performs a
match, returning nonzero (actually, the length of the match) on success,
and `contains' is a boolean function performing either a search or
match, depending on whether an index argument is provided:

`x.Index("lo")'
     returns the zero-based index of the leftmost occurrence of
     substring "lo" (3, in this case).  The argument may be a wxString,
     wxSubString, char, char*, or wxRegex.

`x.Index("l", 2)'
     returns the index of the first of the leftmost occurrence of "l"
     found starting the search at position x[2], or 2 in this case.

`x.Index("l", -1)'
     returns the index of the rightmost occurrence of "l", or 3 here.

`x.Index("l", -3)'
     returns the index of the rightmost occurrence of "l" found by
     starting the search at the 3rd to the last position of x,
     returning 2 in this case.

`pos = r.Search("leo", 3, len, 0)'
     returns the index of r in the `char*' string of length 3, starting
     at position 0, also placing the  length of the match in reference
     parameter len.

`x.Contains("He")'
     returns nonzero if the wxString x contains the substring "He". The
     argument may be a wxString, wxSubString, char, char*, or wxRegex.

`x.Contains("el", 1)'
     returns nonzero if x contains the substring "el" at position 1.
     As in this example, the second argument to `contains', if present,
     means to match the substring only at that position, and not to
     search elsewhere in the string.

`x.Contains(RXwhite);'
     returns nonzero if x contains any whitespace (space, tab, or
     newline). Recall that `RXwhite' is a global whitespace wxRegex.

`x.Matches("lo", 3)'
     returns nonzero if x starting at position 3 exactly matches "lo",
     with no trailing characters (as it does in this example).

`x.Matches(r)'
     returns nonzero if wxString x as a whole matches wxRegex r.

`int f = x.Freq("l")'
     returns the number of distinct, nonoverlapping matches to the
     argument (2 in this case).

Substring extraction
====================

   Substrings may be extracted via the `at', `before', `through',
`from', and `after' functions.  These behave as either lvalues or
rvalues.

`z = x.At(2, 3)'
     sets wxString z to be equal to the length 3 substring of wxString x
     starting at zero-based position 2, setting z to "llo" in this
     case. A nil wxString is returned if the arguments don't make sense.

`x.At(2, 2) = "r"'
     Sets what was in positions 2 to 3 of x to "r", setting x to "Hero"
     in this case. As indicated here, wxSubString assignments may be of
     different lengths.

`x.At("He") = "je";'
     x("He") is the substring of x that matches the first occurrence of
     it's argument. The substitution sets x to "jello". If "He" did not
     occur, the substring would be nil, and the assignment would have
     no effect.

`x.At("l", -1) = "i";'
     replaces the rightmost occurrence of "l" with "i", setting x to
     "Helio".

`z = x.At(r)'
     sets wxString z to the first match in x of wxRegex r, or "ello" in this
     case. A nil wxString is returned if there is no match.

`z = x.Before("o")'
     sets z to the part of x to the left of the first occurrence of
     "o", or "Hell" in this case. The argument may also be a wxString,
     wxSubString, or wxRegex.  (If there is no match, z is set to "".)

`x.Before("ll") = "Bri";'
     sets the part of x to the left of "ll" to "Bri", setting x to
     "Brillo".

`z = x.Before(2)'
     sets z to the part of x to the left of x[2], or "He" in this case.

`z = x.After("Hel")'
     sets z to the part of x to the right of "Hel", or "lo" in this
     case.

`z = x.Through("el")'
     sets z to the part of x up and including "el", or "Hel" in this
     case.

`z = x.From("el")'
     sets z to the part of x from "el" to the end, or "ello" in this
     case.

`x.After("Hel") = "p";'
     sets x to "Help";

`z = x.After(3)'
     sets z to the part of x to the right of x[3] or "o" in this case.

`z = "  ab c"; z = z.After(RXwhite)'
     sets z to the part of its old string to the right of the first
     group of whitespace, setting z to "ab c"; Use GSub(below) to strip
     out multiple occurrences of whitespace or any pattern.

`x[0] = 'J';'
     sets the first element of x to 'J'. x[i] returns a reference to
     the ith element of x, or triggers an error if i is out of range.

`CommonPrefix(x, "Help")'
     returns the wxString containing the common prefix of the two Strings
     or "Hel" in this case.

`CommonSuffix(x, "to")'
     returns the wxString containing the common suffix of the two Strings
     or "o" in this case.

Concatenation
=============

`z = x + s + ' ' + y.At("w") + y.After("w") + ".";'
     sets z to "Hello, world."

`x += y;'
     sets x to "Helloworld"

`Cat(x, y, z)'
     A faster way to say z = x + y.

`Cat(z, y, x, x)'
     Double concatenation; A faster way to say x = z + y + x.

`y.Prepend(x);'
     A faster way to say y = x + y.

`z = Replicate(x, 3);'
     sets z to "HelloHelloHello".

`z = Join(words, 3, "/")'
     sets z to the concatenation of the first 3 Strings in wxString array
     words, each separated by "/", setting z to "a/b/c" in this case.
     The last argument may be "" or 0, indicating no separation.

Other manipulations
===================

`z = "this string has five words"; i = Split(z, words, 10, RXwhite);'
     sets up to 10 elements of wxString array words to the parts of z
     separated by whitespace, and returns the number of parts actually
     encountered (5 in this case). Here, words[0] = "this", words[1] =
     "string", etc.  The last argument may be any of the usual.  If
     there is no match, all of z ends up in words[0]. The words array
     is *not* dynamically created by split.

`int nmatches x.GSub("l","ll")'
     substitutes all original occurrences of "l" with "ll", setting x
     to "Hellllo". The first argument may be any of the usual,
     including wxRegex.  If the second argument is "" or 0, all
     occurrences are deleted. gsub returns the number of matches that
     were replaced.

`z = x + y;  z.Del("loworl");'
     deletes the leftmost occurrence of "loworl" in z, setting z to
     "Held".

`z = Reverse(x)'
     sets z to the reverse of x, or "olleH".

`z = Upcase(x)'
     sets z to x, with all letters set to uppercase, setting z to
     "HELLO"

`z = Downcase(x)'
     sets z to x, with all letters set to lowercase, setting z to
     "hello"

`z = Capitalize(x)'
     sets z to x, with the first letter of each word set to uppercase,
     and all others to lowercase, setting z to "Hello"

`x.Reverse(), x.Upcase(), x.Downcase(), x.Capitalize()'
     in-place, self-modifying versions of the above.

Reading, Writing and Conversion
===============================

`cout << x'
     writes out x.

`cout << x.At(2, 3)'
     writes out the substring "llo".

`cin >> x'
     reads a whitespace-bounded string into x.

`x.Length()'
     returns the length of wxString x (5, in this case).

`s = (const char*)x'
     can be used to extract the `char*' char array. This coercion is
     useful for sending a wxString as an argument to any function
     expecting a `const char*' argument (like `atoi', and
     `File::open'). This operator must be used with care, since the
     conversion returns a pointer to `wxString' internals without copying
     the characters: The resulting `(char*)' is only valid until the
     next wxString operation,  and you must not modify it.  (The
     conversion is defined to return a const value so that GNU C++ will
     produce warning and/or error messages if changes are attempted.)


/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
3. Regular expressions (extracts from GNU docu)
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////

Overview
========

Regular expression matching allows you to test whether a string fits
into a specific syntactic shape.  You can also search a string for a
substring that fits a pattern.

A regular expression describes a set of strings.  The simplest case
is one that describes a particular string; for example, the string
`foo' when regarded as a regular expression matches `foo' and nothing
else.  Nontrivial regular expressions use certain special constructs
so that they can match more than one string.  For example, the
regular expression `foo\|bar' matches either the string `foo' or the
string `bar'; the regular expression `c[ad]*r' matches any of the
strings `cr', `car', `cdr', `caar', `cadddar' and all other such
strings with any number of `a''s and `d''s.

The first step in matching a regular expression is to compile it. 
You must supply the pattern string and also a pattern buffer to hold
the compiled result.  That result contains the pattern in an internal
format that is easier to use in matching.

Having compiled a pattern, you can match it against strings.  You can
match the compiled pattern any number of times against different
strings.


Syntax of Regular Expressions
=============================

Regular expressions have a syntax in which a few characters are
special constructs and the rest are "ordinary".  An ordinary
character is a simple regular expression which matches that character
and nothing else.  The special characters are `$', `^', `.', `*',
`+', `?', `[', `]' and `\'.  Any other character appearing in a
regular expression is ordinary, unless a `\' precedes it.

For example, `f' is not a special character, so it is ordinary, and
therefore `f' is a regular expression that matches the string `f' and
no other string.  (It does *not* match the string `ff'.)  Likewise,
`o' is a regular expression that matches only `o'.

Any two regular expressions A and B can be concatenated.  The result
is a regular expression which matches a string if A matches some
amount of the beginning of that string and B matches the rest of the
string.

As a simple example, we can concatenate the regular expressions `f'
and `o' to get the regular expression `fo', which matches only the
string `fo'.  Still trivial.

Note: for Unix compatibility, special characters are treated as
ordinary ones if they are in contexts where their special meanings
make no sense.  For example, `*foo' treats `*' as ordinary since
there is no preceding expression on which the `*' can act.  It is
poor practice to depend on this behavior; better to quote the special
character anyway, regardless of where is appears.


The following are the characters and character sequences which have
special meaning within regular expressions.  Any character not
mentioned here is not special; it stands for exactly itself for the
purposes of searching and matching.  *Note syntax::.

`.'
     is a special character that matches anything except a newline. 
     Using concatenation, we can make regular expressions like `a.b'
     which matches any three-character string which begins with `a'
     and ends with `b'.

`*'
     is not a construct by itself; it is a suffix, which means the
     preceding regular expression is to be repeated as many times as
     possible.  In `fo*', the `*' applies to the `o', so `fo*'
     matches `f' followed by any number of `o''s.

     The case of zero `o''s is allowed: `fo*' does match `f'.

     `*' always applies to the *smallest* possible preceding
     expression.  Thus, `fo*' has a repeating `o', not a repeating
     `fo'.

     The matcher processes a `*' construct by matching, immediately,
     as many repetitions as can be found.  Then it continues with the
     rest of the pattern.  If that fails, backtracking occurs,
     discarding some of the matches of the `*''d construct in case
     that makes it possible to match the rest of the pattern.  For
     example, matching `c[ad]*ar' against the string `caddaar', the
     `[ad]*' first matches `addaa', but this does not allow the next
     `a' in the pattern to match.  So the last of the matches of
     `[ad]' is undone and the following `a' is tried again.  Now it
     succeeds.

`+'
     `+' is like `*' except that at least one match for the preceding
     pattern is required for `+'.  Thus, `c[ad]+r' does not match
     `cr' but does match anything else that `c[ad]*r' would match.

`?'
     `?' is like `*' except that it allows either zero or one match
     for the preceding pattern.  Thus, `c[ad]?r' matches `cr' or
     `car' or `cdr', and nothing else.

`[ ... ]'
     `[' begins a "character set", which is terminated by a `]'.  In
     the simplest case, the characters between the two form the set. 
     Thus, `[ad]' matches either `a' or `d', and `[ad]*' matches any
     string of `a''s and `d''s (including the empty string), from
     which it follows that `c[ad]*r' matches `car', etc.

     Character ranges can also be included in a character set, by
     writing two characters with a `-' between them.  Thus, `[a-z]'
     matches any lower-case letter.  Ranges may be intermixed freely
     with individual characters, as in `[a-z$%.]', which matches any
     lower case letter or `$', `%' or period.

     Note that the usual special characters are not special any more
     inside a character set.  A completely different set of special
     characters exists inside character sets: `]', `-' and `^'.

     To include a `]' in a character set, you must make it the first
     character.  For example, `[]a]' matches `]' or `a'.  To include
     a `-', you must use it in a context where it cannot possibly
     indicate a range: that is, as the first character, or
     immediately after a range.

`[^ ... ]'
     `[^' begins a "complement character set", which matches any
     character except the ones specified.  Thus, `[^a-z0-9A-Z]'
     matches all characters *except* letters and digits.

     `^' is not special in a character set unless it is the first
     character.  The character following the `^' is treated as if it
     were first (it may be a `-' or a `]').

`^'
     is a special character that matches the empty string -- but only
     if at the beginning of a line in the text being matched. 
     Otherwise it fails to match anything.  Thus, `^foo' matches a
     `foo' which occurs at the beginning of a line.

`$'
     is similar to `^' but matches only at the end of a line.  Thus,
     `xx*$' matches a string of one or more `x''s at the end of a line.

`\'
     has two functions: it quotes the above special characters
     (including `\'), and it introduces additional special constructs.

     Because `\' quotes special characters, `\$' is a regular
     expression which matches only `$', and `\[' is a regular
     expression which matches only `[', and so on.

     For the most part, `\' followed by any character matches only
     that character.  However, there are several exceptions:
     characters which, when preceded by `\', are special constructs. 
     Such characters are always ordinary when encountered on their own.

     No new special characters will ever be defined.  All extensions
     to the regular expression syntax are made by defining new
     two-character constructs that begin with `\'.

`\|'
     specifies an alternative.  Two regular expressions A and B with
     `\|' in between form an expression that matches anything that
     either A or B will match.

     Thus, `foo\|bar' matches either `foo' or `bar' but no other
     string.

     `\|' applies to the largest possible surrounding expressions. 
     Only a surrounding `\( ... \)' grouping can limit the grouping
     power of `\|'.

     Full backtracking capability exists when multiple `\|''s are used.

`\( ... \)'
     is a grouping construct that serves three purposes:

       1. To enclose a set of `\|' alternatives for other operations.
          Thus, `\(foo\|bar\)x' matches either `foox' or `barx'.

       2. To enclose a complicated expression for the postfix `*' to
          operate on.  Thus, `ba\(na\)*' matches `bananana', etc.,
          with any (zero or more) number of `na''s.

       3. To mark a matched substring for future reference.

     This last application is not a consequence of the idea of a
     parenthetical grouping; it is a separate feature which happens
     to be assigned as a second meaning to the same `\( ... \)'
     construct because there is no conflict in practice between the
     two meanings.  Here is an explanation of this feature:

`\DIGIT'
     After the end of a `\( ... \)' construct, the matcher remembers
     the beginning and end of the text matched by that construct. 
     Then, later on in the regular expression, you can use `\'
     followed by DIGIT to mean "match the same text matched the
     DIGIT'th time by the `\( ... \)' construct."  The `\( ... \)'
     constructs are numbered in order of commencement in the regexp.

     The strings matching the first nine `\( ... \)' constructs
     appearing in a regular expression are assigned numbers 1 through
     9 in order of their beginnings.  `\1' through `\9' may be used
     to refer to the text matched by the corresponding `\( ... \)'
     construct.

     For example, `\(.*\)\1' matches any string that is composed of
     two identical halves.  The `\(.*\)' matches the first half,
     which may be anything, but the `\1' that follows must match the
     same exact text.

`\b'
     matches the empty string, but only if it is at the beginning or
     end of a word.  Thus, `\bfoo\b' matches any occurrence of `foo'
     as a separate word.  `\bball\(s\|\)\b' matches `ball' or `balls'
     as a separate word.

`\B'
     matches the empty string, provided it is *not* at the beginning
     or end of a word.

`\<'
     matches the empty string, but only if it is at the beginning of
     a word.

`\>'
     matches the empty string, but only if it is at the end of a word.

`\w'
     matches any word-constituent character.

`\W'
     matches any character that is not a word-constituent.

There are a number of additional `\' regexp directives available for
use within Emacs only.


Constructs Available in Emacs Only
----------------------------------

`\`'
     matches the empty string, but only if it is at the beginning of
     the buffer.

`\''
     matches the empty string, but only if it is at the end of the
     buffer.

`\sCODE'
     matches any character whose syntax is CODE.  CODE is a letter
     which represents a syntax code: thus, `w' for word constituent,
     `-' for whitespace, `(' for open-parenthesis, etc.  See the
     documentation for the Emacs function `modify-syntax-entry' for
     further details.

     Thus, `\s(' matches any character with open-parenthesis syntax.

`\SCODE'
     matches any character whose syntax is not CODE.


