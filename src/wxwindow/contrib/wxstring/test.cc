/*
 * File:	test.cc
 * Purpose:	Demo and test for wxString class library
 * Author:	Stefan Hammes (steve) stefan.hammes@urz.uni-heidelberg.de
 * Copyright:	(c) 1995
 * Created:	1995
 * Updated:	
 * 16.04.95
 * 09.05.95 added test code from GNU docu and for extensions
 * 25.05.95 added test code for old, simple wxWindows string class
 */

#define USE_GNU_WXSTRING 1

#include <stdio.h>
#include <assert.h>
#include "wxregex.h"
#include "wxstrgnu.h"

//////////////////////////////////////////////////////////////
// the code for this test function is simply copied from
// the documentation. the computed values are checked against
// the expected ones. it checks most GNU functions.
//////////////////////////////////////////////////////////////
void testGNU(void)
{
#ifdef wx_msw
// disables warnings about unreferenced parameters and local variables (MSVC++)
#pragma warning( disable : 4100 4101 )
#endif


// Constructors
// ============

//   Strings are initialized and assigned as in the following examples:

{ wxString x; assert(x.IsNull()); 
  wxString y=""; assert(!y.IsNull()); }
//     Set x to the nil string. This is different from the original GNU code
//     which sets a strings also to nil when it is assign 0 or "".


{ wxString x = "Hello"; wxString y("Hello"); assert(x=="Hello" && y=="Hello"); }
//     Set x and y to a copy of the string "Hello".

{ wxString x = 'A'; wxString y('A'); assert(x=="A" && y=="A"); }
//     Set x and y to the string value "A"

{ wxString x="Hello"; wxString u = x; wxString v(x); assert(u==x && v==x); }
//     Set u and v to the same string as wxString x

{ wxString x="Hello"; wxString u = x.At(1,4); wxString v(x.At(1,4)); assert(u=="ello" && v=="ello"); }
//     Set u and v to the length 4 substring of x starting at position 1
//     (counting indexes from 0).

{ wxString x("abc", 2); assert(x=="ab"); }
//     Sets x to "ab", i.e., the first 2 characters of "abc".

{ char buf[20]; wxString x = strcpy(buf,"20"); assert(x=="20"); }
//     Sets x to "20". As here, Strings may be initialized or assigned
//     the results of any char*' function.


// used to have the variables fresh each time
#define VARIABLES \
  wxString x = "Hello"; \
  wxString y = "world"; \
  wxString n = "123"; \
  wxString z; \
  char*  s = ","; \
  wxString lft, mid, rgt; \
  wxRegex  r("e[a-z]*o"); \
  wxRegex  r2("/[a-z]*/"); \
  char   c; \
  int    i, pos, len; \
  double f; \
  wxString words[10]; \
  words[0] = "a"; \
  words[1] = "b"; \
  words[2] = "c"; 

{ VARIABLES;  i = x.Index("lo"); assert(i==3); }
//     returns the zero-based index of the leftmost occurrence of
//     substring "lo" (3, in this case).  The argument may be a wxString,
//     wxSubString, char, char*, or wxRegex.

{ VARIABLES;  i = x.Index("l", 2); assert(i==2); }
//     returns the index of the first of the leftmost occurrence of "l"
//     found starting the search at position x[2], or 2 in this case.

{ VARIABLES;  i = x.Index("l", -1); assert(i==3); }
//     returns the index of the rightmost occurrence of "l", or 3 here.

{ VARIABLES;  i = x.Index("l", -3); assert(i==2); }
//     returns the index of the rightmost occurrence of "l" found by
//     starting the search at the 3rd to the last position of x,
//     returning 2 in this case.

{ VARIABLES;  pos = r.Search("leo", 3, len, 0); assert(pos==1 && len==2); }
//     returns the index of r in the `char*' string of length 3, starting
//     at position 0, also placing the  length of the match in reference
//     parameter len.

{ VARIABLES;  i = x.Contains("He"); assert(i==1); }
//     returns nonzero if the wxString x contains the substring "He". The
//     argument may be a wxString, wxSubString, char, char*, or wxRegex.

{ VARIABLES;  i = x.Contains("el", 1); assert(i==1); }
//     returns nonzero if x contains the substring "el" at position 1.
//     As in this example, the second argument to `contains', if present,
//     means to match the substring only at that position, and not to
//     search elsewhere in the string.

{ VARIABLES;  i = x.Contains(RXwhite); assert(i==0); }
//     returns nonzero if x contains any whitespace (space, tab, or
//     newline). Recall that `RXwhite' is a global whitespace wxRegex.

{ VARIABLES;  i = x.Matches("lo", 3); assert(i==1); }
//     returns nonzero if x starting at position 3 exactly matches "lo",
//     with no trailing characters (as it does in this example).

{ VARIABLES;  i = x.Matches(r); assert(i==0); }
//     returns nonzero if wxString x as a whole matches wxRegex r.

{ VARIABLES;  i = x.Freq("l"); assert(i==2); }
//     returns the number of distinct, nonoverlapping matches to the
//     argument (2 in this case).

// Substring extraction
// ====================

//    Substrings may be extracted via the `at', `before', `through',
// `from', and `after' functions.  These behave as either lvalues or
// rvalues.

{ VARIABLES;  z = x.At(2, 3); assert(z=="llo"); }
//     sets wxString z to be equal to the length 3 substring of wxString x
//     starting at zero-based position 2, setting z to "llo" in this
//     case. A nil wxString is returned if the arguments don't make sense.

{ VARIABLES;  x.At(2, 2) = "r"; assert(x=="Hero"); }
//     Sets what was in positions 2 to 3 of x to "r", setting x to "Hero"
//     in this case. As indicated here, wxSubString assignments may be of
//     different lengths.

{ VARIABLES;  x.At("He") = "je"; assert(x=="jello"); }
//     x("He") is the substring of x that matches the first occurrence of
//     it's argument. The substitution sets x to "jello". If "He" did not
//     occur, the substring would be nil, and the assignment would have
//     no effect.

{ VARIABLES;  x.At("l", -1) = "i"; assert(x=="Helio"); }
//     replaces the rightmost occurrence of "l" with "i", setting x to
//     "Helio".

{ VARIABLES;  z = x.At(r); assert(z=="ello"); }
//     sets wxString z to the first match in x of wxRegex r, or "ello" in this
//     case. A nil wxString is returned if there is no match.

{ VARIABLES;  z = x.Before("o"); assert(z=="Hell"); }
//     sets z to the part of x to the left of the first occurrence of
//     "o", or "Hell" in this case. The argument may also be a wxString,
//     wxSubString, or wxRegex.  (If there is no match, z is set to "".)

{ VARIABLES;  x.Before("ll") = "Bri"; assert(x=="Brillo"); }
//     sets the part of x to the left of "ll" to "Bri", setting x to
//     "Brillo".

{ VARIABLES;  z = x.Before(2); assert(z=="He"); }
//     sets z to the part of x to the left of x[2], or "He" in this case.

{ VARIABLES;  z = x.After("Hel"); assert(z=="lo"); }
//     sets z to the part of x to the right of "Hel", or "lo" in this
//     case.

{ VARIABLES;  z = x.Through("el"); assert(z=="Hel"); }
//     sets z to the part of x up and including "el", or "Hel" in this
//     case.

{ VARIABLES;  z = x.From("el"); assert(z=="ello"); }
//     sets z to the part of x from "el" to the end, or "ello" in this
//     case.

{ VARIABLES;  x.After("Hel") = "p"; assert(x=="Help"); }
//     sets x to "Help";

{ VARIABLES;  z = x.After(3); assert(z=="o"); }
//     sets z to the part of x to the right of x[3] or "o" in this case.

{ VARIABLES;  z = "  ab c"; z = z.After(RXwhite); assert(z=="ab c"); }
//     sets z to the part of its old string to the right of the first
//     group of whitespace, setting z to "ab c"; Use gsub(below) to strip
//     out multiple occurrences of whitespace or any pattern.

{ VARIABLES;  x[0] = 'J'; assert(x=="Jello"); }
//     sets the first element of x to 'J'. x[i] returns a reference to
//     the ith element of x, or triggers an error if i is out of range.

{ VARIABLES;  z = CommonPrefix(x, "Help"); assert(z=="Hel"); }
//     returns the wxString containing the common prefix of the two Strings
//     or "Hel" in this case.

{ VARIABLES;  z = CommonSuffix(x, "to"); assert(z=="o"); }
//     returns the wxString containing the common suffix of the two Strings
//     or "o" in this case.

// Concatenation
// =============

{ VARIABLES;  z = x + s + ' ' + y.At("w") + y.After("w") + "."; assert(z=="Hello, world."); }
//     sets z to "Hello, world."

{ VARIABLES;  x += y; assert(x=="Helloworld"); }
//     sets x to "Helloworld"

{ VARIABLES;  Cat(x, y, z); assert(z=="Helloworld"); }
//     A faster way to say z = x + y.

{ VARIABLES;  Cat(z, y, x, x); assert(x=="worldHello"); }
//     Double concatenation; A faster way to say x = z + y + x.

{ VARIABLES;  y.Prepend(x); assert(y=="Helloworld"); }
//     A faster way to say y = x + y.

{ VARIABLES;  z = Replicate(x, 3); assert(z=="HelloHelloHello"); }
//     sets z to "HelloHelloHello".

#ifndef VMS
{ VARIABLES;  z = Join(words, 3, "/"); assert(z=="a/b/c"); }
//     sets z to the concatenation of the first 3 Strings in wxString array
//     words, each separated by "/", setting z to "a/b/c" in this case.
//     The last argument may be "" or 0, indicating no separation.
#endif

// Other manipulations
// ===================

#ifndef VMS
{ VARIABLES;  z = "this string has five words"; i = Split(z, words, 10, RXwhite); 
              assert(i==5 && words[0]=="this" && words[1]=="string" && words[2]=="has" &&
                     words[3]=="five" && words[4]=="words"); }
//     sets up to 10 elements of wxString array words to the parts of z
//     separated by whitespace, and returns the number of parts actually
//     encountered (5 in this case). Here, words[0] = "this", words[1] =
//     "string", etc.  The last argument may be any of the usual.  If
//     there is no match, all of z ends up in words[0]. The words array
//     is *not* dynamically created by split.
#endif

{ VARIABLES;  int nmatches = x.GSub("l","ll"); assert(x=="Hellllo" && nmatches==2); }
//     substitutes all original occurrences of "l" with "ll", setting x
//     to "Hellllo". The first argument may be any of the usual,
//     including wxRegex.  If the second argument is "" or 0, all
//     occurrences are deleted. gsub returns the number of matches that
//     were replaced.

{ VARIABLES;  z = x + y;  z.Del("loworl"); assert(z=="Held"); }
//     deletes the leftmost occurrence of "loworl" in z, setting z to
//     "Held".

{ VARIABLES;  z = Reverse(x); assert(z=="olleH"); }
//     sets z to the reverse of x, or "olleH".

{ VARIABLES;  z = Upcase(x); assert(z=="HELLO"); }
//     sets z to x, with all letters set to uppercase, setting z to
//     "HELLO"

{ VARIABLES;  z = Downcase(x); assert(z=="hello"); }
//     sets z to x, with all letters set to lowercase, setting z to
//     "hello"

{ VARIABLES;  z = Capitalize(x); assert(z=="Hello"); }
//     sets z to x, with the first letter of each word set to uppercase,
//     and all others to lowercase, setting z to "Hello"

{ VARIABLES;  x.Reverse(); assert(x=="olleH"); }
{ VARIABLES;  x.Upcase(); assert(x=="HELLO"); }
{ VARIABLES;  x.Downcase(); assert(x=="hello"); }
{ VARIABLES;  x.Capitalize(); assert(x=="Hello"); }
//     in-place, self-modifying versions of the above.

// Reading, Writing and Conversion
// ===============================

{ VARIABLES;  cout << x; cout<<"\nOutput should be: `Hello'\n"; }
//     writes out x.

{ VARIABLES;  cout << x.At(2, 3); cout<<"\nOutput should be: `llo'\n"; }
//     writes out the substring "llo".

{ VARIABLES;  cout<<"Input a whitespace-bounded string: "; cin >> x; 
              cout<<"You have input `"<<x<<"'\n"; }
//     reads a whitespace-bounded string into x.

{ VARIABLES;  i = x.Length(); assert(i==5); }
//     returns the length of wxString x (5, in this case).

// doesn't work!  s = (const char*)x;
//     can be used to extract the `char*' char array. This coercion is
//     useful for sending a wxString as an argument to any function
//     expecting a `const char*' argument (like `atoi', and
//     `File::open'). This operator must be used with care, since the
//     conversion returns a pointer to `wxString' internals without copying
//     the characters: The resulting `(char*)' is only valid until the
//     next wxString operation,  and you must not modify it.  (The
//     conversion is defined to return a const value so that GNU C++ will
//     produce warning and/or error messages if changes are attempted.)
}






//////////////////////////////////////////////////////////////
// show and test features of the wxString class which are
// implemented by Stefan Hammes
//////////////////////////////////////////////////////////////
wxString extract(wxString& s, wxRegex &re)
{
  return(((wxString)s.At(re)).Through(re));
}

void testSteve(void)
{
  wxString s;
  
  s.sprintf("Line #%d",23); assert(s=="Line #23");
  
  s=" Hello  ";
  assert(s.Strip()==" Hello");
  assert(s.Strip(wxString::leading)=="Hello  ");
  assert(s.Strip(wxString::both)=="Hello");
  s=">Hello>>";
  assert(s.Strip(wxString::trailing,'>')==">Hello");
  assert(s.Strip(wxString::leading,'>')=="Hello>>");
  assert(s.Strip(wxString::both,'>')=="Hello");
  
  s="Hello";
  s.RemoveLast();
  assert(s=="Hell");
  
  s="Hello";
  assert(s.IsAscii());
  assert(s.IsWord());
  assert(!s.IsNumber());
  assert(!s.IsNull());
  assert(s.IsDefined());
  
  int i=strlen(s); assert(i==5); // conversion to (const char *)
  
  s = "WINDOW.NAME(demo)";
  wxRegex re("[A-Z]+");
  wxString msg;
  msg.sprintf("`%s' contains = %d, substring = `%s'\n",
              s.Chars(),s.Contains(re),extract(s,re).Chars());
  cout<<msg;              
  wxRegex re1("([a-z]*)");
  msg.sprintf("`%s' contains = %d, substring = `%s'\n",
              s.Chars(),s.Contains(re1),extract(s,re1).Chars());
  cout<<msg;              
  
  // the wxCHARARG macro is necessary in most wxWindows functions,
  // because they assume (char *) and not (const char *)
  // e.g.: 
  //   wxString s="Title";
  //   frame->SetTitle(wxCHARARG(s));

  s="Hello";
  assert(strcmp(s,s.GetData())==0);
  assert(s==s.Copy());
  
  assert(s.CompareTo("Hello")==0);
  assert(s.CompareTo("hello")!=0);
  assert(s.CompareTo("hello",wxString::ignoreCase)==0);
  assert(s.CompareTo(s)==0);  
  assert(s<"hello");
  assert(s(2)=='l');       
  
  s.Append(" world");
  assert(s=="Hello world");
  assert(s.Contains("Hello"));
  assert(!s.Contains("hello"));
  assert(s.Contains("hello",wxString::ignoreCase));
  
  assert(s.First("Hello")==0);
  assert(s.First("world")==6);
  
  assert(s.Index("world")==6);
  assert(s.Index("World")==NO_POS);
  assert(s.Index("World",0,wxString::ignoreCase)==6);

  s.Insert(5," nice");
  assert(s=="Hello nice world");
  
  s="1232123";            
  assert(s.Last("1")==4);
  
  s="ABC";
  s.Append('*',5);
  assert(s=="ABC*****");
  s.Prepend('*',5);
  assert(s=="*****ABC*****");
  
  s="ABC";
  s.RemoveLast();
  assert(s=="AB");
  
  s="ABCDEF";
  s.Remove(2);
  assert(s=="AB");
  s="ABCDEF";
  s.Remove(2,1);
  assert(s=="ABDEF");
  
  s="ABCDEF";
  s.Replace(2,1,"X");
  assert(s=="ABXDEF");
  
  s="ABCDEF";
  s.LowerCase();
  assert(s=="abcdef");
  s.UpperCase();
  assert(s=="ABCDEF");
  
  assert(s.SubString(1,4)=="BCDE");
}








int main(void)
{
  testGNU();
  testSteve();
  return(0);
}

