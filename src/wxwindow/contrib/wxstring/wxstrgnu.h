// This file was adapted to usage in non-GNU environments
// (especially under wxWindows) by Stefan Hammes (steve),
// (stefan.hammes@urz.uni-heidelberg.de)
//
// There were also some minor extensions made.
//
// To use this code indepently from libg++ all classes
// were renamed by adding a 'wx' in front of their names!
//
// 01.12.94
//
// NOTE: the DEC C++ compiler doesn't allow friend members to be
//       declared NOT inline in the class and inline afterwards.
//       therefore add the inline qualifier to the declarations
//       in the class.
// NOTE: the DEC C++ compiler doesn't allow array parameters of the
//       class in definition. therefore the Split/Join members are
//       not available under VMS
// Stefan Hammes (steve), 03.12.94
//
// 25.05.95: all member functions capitalized to fit naming convention
//           of wxWindows. added member functions from the simple string
//           class of wxWindows. changed void return types to return
//           a reference to 'this'.

#ifdef wx_msw
#define VMSinline /**/ 
#else
#define VMSinline inline
#endif

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


#ifndef _wxString_h
#ifdef __GNUG__
#pragma interface
#endif
#define _wxString_h 1

#include <iostream.h>
#include <stdio.h> // for Readline

#include "wx_obj.h"

#ifdef VMS
// this is included in [.contrib.wxstring]wxstring.h under VMS,
// but in \include\base\wxstring.h under DOS or UNIX
#include "wxregex.h"
#else
// must be done separately in wxstring.cc before including wxstring.h #include "wxregex.h"
#endif

struct wxStrRep                     // internal wxString representations
{
  unsigned short    len;         // string length 
  unsigned short    sz;          // allocated space
  char              s[1];        // the string starts here 
                                 // (at least 1 char for trailing null)
                                 // allocated & expanded via non-public fcts
};

// primitive ops on StrReps -- nearly all wxString fns go through these.

wxStrRep*     Salloc(wxStrRep*, const char*, int, int);
wxStrRep*     Scopy(wxStrRep*, const wxStrRep*);
wxStrRep*     Sresize(wxStrRep*, int);
wxStrRep*     Scat(wxStrRep*, const char*, int, const char*, int);
wxStrRep*     Scat(wxStrRep*, const char*, int,const char*,int, const char*,int);
wxStrRep*     Sprepend(wxStrRep*, const char*, int);
wxStrRep*     Sreverse(const wxStrRep*, wxStrRep*);
wxStrRep*     Supcase(const wxStrRep*, wxStrRep*);
wxStrRep*     Sdowncase(const wxStrRep*, wxStrRep*);
wxStrRep*     Scapitalize(const wxStrRep*, wxStrRep*);

// These classes need to be defined in the order given

class wxString;
class wxSubString;           

class wxSubString            
#if defined(wx_msw) || defined(wx_motif)
  : public wxObject
#endif
{
  friend class      wxString;
protected:

  wxString&           S;        // The wxString I'm a substring of
  unsigned short    pos;      // starting position in S's rep
  unsigned short    len;      // length of substring

  void              assign(const wxStrRep*, const char*, int = -1);
                    wxSubString(wxString& x, int p, int l);
                    wxSubString(const wxSubString& x);

public:

// Note there are no public constructors. SubStrings are always
// created via wxString operations

                   ~wxSubString();

  wxSubString&      operator =  (const wxString&     y);
  wxSubString&      operator =  (const wxSubString&  y);
  wxSubString&      operator =  (const char* t);
  wxSubString&      operator =  (char        c);

// return 1 if target appears anywhere in wxSubString; else 0

  int               Contains(char        c) const;
  int               Contains(const wxString&     y) const;
  int               Contains(const wxSubString&  y) const;
  int               Contains(const char* t) const;
  int               Contains(const wxRegex&       r) const;

// return 1 if target matches entire wxSubString

  int               Matches(const wxRegex&  r) const;

// IO 

  friend ostream&   operator<<(ostream& s, const wxSubString& x);

// status

  unsigned int      Length() const;
  int               Empty() const;
  const char*       Chars() const;

  int               OK() const; 

};

class wxString
#if defined(wx_msw) || defined(wx_motif)
  : public wxObject
#endif
{
  friend class      wxSubString;

protected:
  wxStrRep*         rep;   // Strings are pointers to their representations

// some helper functions

  int               Search(int, int, const char*, int = -1) const;
  int               Search(int, int, char) const;
  int               Match(int, int, int, const char*, int = -1) const;
  int               _gsub(const char*, int, const char* ,int);
  int               _gsub(const wxRegex&, const char*, int);
  wxSubString       _substr(int, int);

public:

// constructors & assignment

                    wxString(void);
                    wxString(const wxString& x);
                    wxString(const wxSubString&  x);
                    wxString(const char* t);
                    wxString(const char* t, int len);
                    wxString(char c);

                   ~wxString();

  wxString&         operator =  (const wxString&     y);
  wxString&         operator =  (const char* y);
  wxString&         operator =  (char        c);
  wxString&         operator =  (const wxSubString&  y);
                    
  wxString          Copy() const;


// replication      

  friend wxString   Replicate(char        c, int n);
  friend wxString   Replicate(const wxString&     y, int n);

// concatenation

  wxString&         operator += (const wxString&     y); 
  wxString&         operator += (const wxSubString&  y);
  wxString&         operator += (const char* t);
  wxString&         operator += (char        c);

  wxString&         Append(const char* cs){ return(*this += cs); }
  wxString&         Append(const wxString& s){ return(*this += s); }
  wxString&         Append(char c, int rep=1){ return(*this += Replicate(c,rep)); }   // Append c rep times

  wxString&         Prepend(const wxString&     y); 
  wxString&         Prepend(const wxSubString&  y);
  wxString&         Prepend(const char* t);
  wxString&         Prepend(char        c);
  wxString&         Prepend(char c, int rep=1){ Prepend(Replicate(c,rep)); return(*this); }  // Prepend c rep times
                                          
// procedural versions:
// concatenate first 2 args, store result in last arg

  VMSinline friend void     Cat(const wxString&, const wxString&, wxString&);
  VMSinline friend void     Cat(const wxString&, const wxSubString&, wxString&);
  VMSinline friend void     Cat(const wxString&, const char*, wxString&);
  VMSinline friend void     Cat(const wxString&, char, wxString&);

  VMSinline friend void     Cat(const wxSubString&, const wxString&, wxString&);
  VMSinline friend void     Cat(const wxSubString&, const wxSubString&, wxString&);
  VMSinline friend void     Cat(const wxSubString&, const char*, wxString&);
  VMSinline friend void     Cat(const wxSubString&, char, wxString&);

  VMSinline friend void     Cat(const char*, const wxString&, wxString&);
  VMSinline friend void     Cat(const char*, const wxSubString&, wxString&);
  VMSinline friend void     Cat(const char*, const char*, wxString&);
  VMSinline friend void     Cat(const char*, char, wxString&);

// double concatenation, by request. (yes, there are too many versions, 
// but if one is supported, then the others should be too...)
// Concatenate first 3 args, store in last arg

  VMSinline friend void     Cat(const wxString&,const wxString&, const wxString&,wxString&);
  VMSinline friend void     Cat(const wxString&,const wxString&,const wxSubString&,wxString&);
  VMSinline friend void     Cat(const wxString&,const wxString&, const char*, wxString&);
  VMSinline friend void     Cat(const wxString&,const wxString&, char, wxString&);
  VMSinline friend void     Cat(const wxString&,const wxSubString&,const wxString&,wxString&);
  VMSinline friend void     Cat(const wxString&,const wxSubString&,const wxSubString&,wxString&);
  VMSinline friend void     Cat(const wxString&,const wxSubString&, const char*, wxString&);
  VMSinline friend void     Cat(const wxString&,const wxSubString&, char, wxString&);
  VMSinline friend void     Cat(const wxString&,const char*, const wxString&,    wxString&);
  VMSinline friend void     Cat(const wxString&,const char*, const wxSubString&, wxString&);
  VMSinline friend void     Cat(const wxString&,const char*, const char*, wxString&);
  VMSinline friend void     Cat(const wxString&,const char*, char, wxString&);

  VMSinline friend void     Cat(const char*, const wxString&, const wxString&,wxString&);
  VMSinline friend void     Cat(const char*,const wxString&,const wxSubString&,wxString&);
  VMSinline friend void     Cat(const char*,const wxString&, const char*, wxString&);
  VMSinline friend void     Cat(const char*,const wxString&, char, wxString&);
  VMSinline friend void     Cat(const char*,const wxSubString&,const wxString&,wxString&);
  VMSinline friend void     Cat(const char*,const wxSubString&,const wxSubString&,wxString&);
  VMSinline friend void     Cat(const char*,const wxSubString&, const char*, wxString&);
  VMSinline friend void     Cat(const char*,const wxSubString&, char, wxString&);
  VMSinline friend void     Cat(const char*,const char*, const wxString&,    wxString&);
  VMSinline friend void     Cat(const char*,const char*, const wxSubString&, wxString&);
  VMSinline friend void     Cat(const char*,const char*, const char*, wxString&);
  VMSinline friend void     Cat(const char*,const char*, char, wxString&);


// searching & matching

// additions for compatibility with the simple wxWindows string class.
// the member functions of the simple wxString class are modelled by
// using the GNU string functions. therefore they will be realized
// much more efficient. functions of the simple wxString class which
// are identical to functions of the GNU string class are left out
// here.
// some functions are left out, because they are too dangerous:

#define NO_POS ((int)(-1)) // undefined position
  enum CaseCompare {exact, ignoreCase};
  
// comparison (more: see below)
  int               CompareTo(const char* cs,     CaseCompare cmp = exact) const;
  int               CompareTo(const wxString& st, CaseCompare cmp = exact) const;

// return position of target in string or -1 for failure

  int               Index(char        c, int startpos = 0) const;      
  int               Index(const wxString&     y, int startpos = 0) const;
  int               Index(const wxString&     y, int startpos, CaseCompare cmp) const;      
  int               Index(const wxSubString&  y, int startpos = 0) const;      
  int               Index(const char* t, int startpos = 0) const;  
  int               Index(const char* t, int startpos, CaseCompare cmp) const;  
  int               Index(const wxRegex&      r, int startpos = 0) const;       

// return 1 if target appears anyhere in wxString; else 0

  Bool              Contains(char        c) const;
  Bool              Contains(const wxString&     y) const;
  Bool              Contains(const wxSubString&  y) const;
  Bool              Contains(const char* t) const;
  Bool              Contains(const wxRegex&      r) const;

// case dependent/independent variation  
  Bool              Contains(const char* pat,     CaseCompare cmp) const;
  Bool              Contains(const wxString& pat, CaseCompare cmp) const;
  

// return 1 if target appears anywhere after position pos 
// (or before, if pos is negative) in wxString; else 0

  Bool              Contains(char        c, int pos) const;
  Bool              Contains(const wxString&     y, int pos) const;
  Bool              Contains(const wxSubString&  y, int pos) const;
  Bool              Contains(const char* t, int pos) const;
  Bool              Contains(const wxRegex&      r, int pos) const;

// return 1 if target appears at position pos in wxString; else 0

  Bool              Matches(char        c, int pos = 0) const;
  Bool              Matches(const wxString&     y, int pos = 0) const;
  Bool              Matches(const wxSubString&  y, int pos = 0) const;
  Bool              Matches(const char* t, int pos = 0) const;
  Bool              Matches(const wxRegex&      r, int pos = 0) const;

//  return number of occurences of target in wxString

  int               Freq(char        c) const; 
  int               Freq(const wxString&     y) const;
  int               Freq(const wxSubString&  y) const;
  int               Freq(const char* t) const;

// first or last occurrence of item
  int               First(char c) const;
  int               First(const char* cs) const;
  int               First(const wxString& cs) const;
  
  int               Last(char c) const;
  int               Last(const char* cs) const;
  int               Last(const wxString& cs) const;

// wxSubString extraction

// Note that you can't take a substring of a const wxString, since
// this leaves open the possiblility of indirectly modifying the
// wxString through the wxSubString


  wxSubString       At(int         pos, int len);
  wxSubString       operator () (int         pos, int len); // synonym for at

  wxSubString       At(const wxString&     x, int startpos = 0); 
  wxSubString       At(const wxSubString&  x, int startpos = 0); 
  wxSubString       At(const char* t, int startpos = 0);
  wxSubString       At(char        c, int startpos = 0);
  wxSubString       At(const wxRegex&      r, int startpos = 0); 

  wxSubString       Before(int          pos);
  wxSubString       Before(const wxString&      x, int startpos = 0);
  wxSubString       Before(const wxSubString&   x, int startpos = 0);
  wxSubString       Before(const char*  t, int startpos = 0);
  wxSubString       Before(char         c, int startpos = 0);
  wxSubString       Before(const wxRegex&       r, int startpos = 0);

  wxSubString       Through(int          pos);
  wxSubString       Through(const wxString&      x, int startpos = 0);
  wxSubString       Through(const wxSubString&   x, int startpos = 0);
  wxSubString       Through(const char*  t, int startpos = 0);
  wxSubString       Through(char         c, int startpos = 0);
  wxSubString       Through(const wxRegex&       r, int startpos = 0);

  wxSubString       From(int          pos);
  wxSubString       From(const wxString&      x, int startpos = 0);
  wxSubString       From(const wxSubString&   x, int startpos = 0);
  wxSubString       From(const char*  t, int startpos = 0);
  wxSubString       From(char         c, int startpos = 0);
  wxSubString       From(const wxRegex&       r, int startpos = 0);

  wxSubString       After(int         pos);
  wxSubString       After(const wxString&     x, int startpos = 0);
  wxSubString       After(const wxSubString&  x, int startpos = 0);
  wxSubString       After(const char* t, int startpos = 0);
  wxSubString       After(char        c, int startpos = 0);
  wxSubString       After(const wxRegex&      r, int startpos = 0);

  // Edward Z.'s additions
  wxString          SubString(int from, int to){ return(At(from,to-from+1)); }

// deletion

// delete len chars starting at pos
  wxString&         Del(int         pos, int len);

// delete the first occurrence of target after startpos

  wxString&         Del(const wxString&     y, int startpos = 0);
  wxString&         Del(const wxSubString&  y, int startpos = 0);
  wxString&         Del(const char* t, int startpos = 0);
  wxString&         Del(char        c, int startpos = 0);
  wxString&         Del(const wxRegex&      r, int startpos = 0);
  wxString&         RemoveLast(void) { Del((int)(Length()-1),(int)1); return(*this); } // the funny casting is necessary to distinguish between the 'Del' members  
  wxString&         Remove(int pos){ Del(pos,(int)(Length()-pos)); return(*this); }         // Remove pos to end of string
  wxString&         Remove(int pos, int len){ Del(pos,len); return(*this); }         // Remove pos to end of string


// insertion
  wxString&         Insert(int pos, const char*);
  wxString&         Insert(int pos, const wxString&);
  
// global substitution: substitute all occurrences of pat with repl (return number of matches)

  int               GSub(const wxString&     pat, const wxString&     repl);
  int               GSub(const wxSubString&  pat, const wxString&     repl);
  int               GSub(const char* pat, const wxString&     repl);
  int               GSub(const char* pat, const char* repl);
  int               GSub(const wxRegex&      pat, const wxString&     repl);

  wxString&         Replace(int pos, int n, const char*);
  wxString&         Replace(int pos, int n, const wxString&);


// friends & utilities

// Split string into array res at separators; return number of elements
#ifndef VMS
  // VMS doesn't allow the arrays here, (steve) 03.12.94
  friend int        Split(const wxString& x, wxString res[], int maxn, 
                          const wxString& sep);
  friend int        Split(const wxString& x, wxString res[], int maxn, 
                          const wxRegex&  sep);
  friend wxString   Join(wxString src[], int n, const wxString& sep);
#endif

  friend wxString   CommonPrefix(const wxString& x, const wxString& y, 
                                  int startpos = 0);
  friend wxString   CommonSuffix(const wxString& x, const wxString& y, 
                                  int startpos = -1);

// strip chars at the front and/or end
// stripType is defined for bitwise ORing!
  enum              StripType {leading = 0x1, trailing = 0x2, both = 0x3};
  wxSubString       Strip(StripType s=trailing, char c=' ');

// simple builtin transformations

  VMSinline friend wxString     Reverse(const wxString& x);
  VMSinline friend wxString     Upcase(const wxString& x);
  VMSinline friend wxString     Downcase(const wxString& x);
  VMSinline friend wxString     Capitalize(const wxString& x);

// in-place versions of above

  void              Reverse();
  void              Upcase();
  void              UpperCase(){ Upcase(); } //steve
  void              Downcase();
  void              LowerCase(){ Downcase(); } //steve
  void              Capitalize();

// element extraction

  char&             operator [] (int i);
  char&             operator()(int pos){ return((*this)[(int)pos]); }
  char              Elem(int i) const;
  char              Firstchar() const;
  char              Lastchar() const;

// conversion

                    operator const char*() const;
  const char*       Chars() const;
  char*             GetData(void);  // wxWindows compatibility

// IO

// formatted assignment
// we do not use the 'sprintf' constructor anymore, because with that
// constructor, every initialisation with a string would go through
// sprintf and this is not desireable, because sprintf interprets
// some characters. with the above function we can write i.e.:
//
// wxString msg; msg.sprintf("Processing item %d\n",count);
  void              sprintf(const char *fmt, ...);

  VMSinline friend ostream&   operator<<(ostream& s, const wxString& x);
  friend ostream&   operator<<(ostream& s, const wxSubString& x);
  friend istream&   operator>>(istream& s, wxString& x);

  friend int        Readline(istream& s, wxString& x, 
                             char terminator = '\n',
                             int discard_terminator = 1);
  friend int        Readline(FILE *f, wxString& x, 
                             char terminator = '\n',
                             int discard_terminator = 1);

// status

  unsigned int      Length(void) const;
  int               Empty(void) const;

// classification (should be capital, because of ctype.h macros!)
  int               IsAscii(void) const;
  int               IsWord(void) const;
  int               IsNumber(void) const;
  int               IsNull(void) const;
  int               IsDefined(void) const { return(! IsNull()); }

// preallocate some space for wxString
  void              Alloc(int newsize);

// report current allocation (not length!)
  int               Allocation(void) const;

  void              Error(const char* msg) const;
  int               OK() const;
  
// here is a very, very, very ugly macro, but it makes things more
// transparent in cases, where a library function requires a 
// (char *) argument. this is especially the case in wxWindows,
// where all char-arguments are (char *) and not (const char *).
// this macro should only be used in such cases and NOT to
// modify the internal data.
// the conventional way would be 'function((char *)string.Chars())'.
// with the macro this can be achieved by 'function(wxCHARARG(string))'.
// this makes it better clear, that the usage should be confined
// to arguments!
#define wxCHARARG(s) ((char *)(s).Chars())  
};

typedef wxString StrTmp; // for backward compatibility

// other externs

int        Compare(const wxString&    x, const wxString&     y);
int        Compare(const wxString&    x, const wxSubString&  y);
int        Compare(const wxString&    x, const char* y);
int        Compare(const wxSubString& x, const wxString&     y);
int        Compare(const wxSubString& x, const wxSubString&  y);
int        Compare(const wxSubString& x, const char* y);
int        FCompare(const wxString&   x, const wxString&     y); // ignore case

extern wxStrRep  _nilStrRep;
extern wxString _nilString;

// other inlines

VMSinline wxString operator + (const wxString& x, const wxString& y);
VMSinline wxString operator + (const wxString& x, const wxSubString& y);
VMSinline wxString operator + (const wxString& x, const char* y);
VMSinline wxString operator + (const wxString& x, char y);
VMSinline wxString operator + (const wxSubString& x, const wxString& y);
VMSinline wxString operator + (const wxSubString& x, const wxSubString& y);
VMSinline wxString operator + (const wxSubString& x, const char* y);
VMSinline wxString operator + (const wxSubString& x, char y);
VMSinline wxString operator + (const char* x, const wxString& y);
VMSinline wxString operator + (const char* x, const wxSubString& y);

VMSinline int operator==(const wxString& x, const wxString& y); 
VMSinline int operator!=(const wxString& x, const wxString& y);
VMSinline int operator> (const wxString& x, const wxString& y);
VMSinline int operator>=(const wxString& x, const wxString& y);
VMSinline int operator< (const wxString& x, const wxString& y);
VMSinline int operator<=(const wxString& x, const wxString& y);
VMSinline int operator==(const wxString& x, const wxSubString&  y);
VMSinline int operator!=(const wxString& x, const wxSubString&  y);
VMSinline int operator> (const wxString& x, const wxSubString&  y);
VMSinline int operator>=(const wxString& x, const wxSubString&  y);
VMSinline int operator< (const wxString& x, const wxSubString&  y);
VMSinline int operator<=(const wxString& x, const wxSubString&  y);
VMSinline int operator==(const wxString& x, const char* t);
VMSinline int operator!=(const wxString& x, const char* t);
VMSinline int operator> (const wxString& x, const char* t);
VMSinline int operator>=(const wxString& x, const char* t);
VMSinline int operator< (const wxString& x, const char* t);
VMSinline int operator<=(const wxString& x, const char* t);
VMSinline int operator==(const wxSubString& x, const wxString& y);
VMSinline int operator!=(const wxSubString& x, const wxString& y);
VMSinline int operator> (const wxSubString& x, const wxString& y);
VMSinline int operator>=(const wxSubString& x, const wxString& y);
VMSinline int operator< (const wxSubString& x, const wxString& y);
VMSinline int operator<=(const wxSubString& x, const wxString& y);
VMSinline int operator==(const wxSubString& x, const wxSubString&  y);
VMSinline int operator!=(const wxSubString& x, const wxSubString&  y);
VMSinline int operator> (const wxSubString& x, const wxSubString&  y);
VMSinline int operator>=(const wxSubString& x, const wxSubString&  y);
VMSinline int operator< (const wxSubString& x, const wxSubString&  y);
VMSinline int operator<=(const wxSubString& x, const wxSubString&  y);
VMSinline int operator==(const wxSubString& x, const char* t);
VMSinline int operator!=(const wxSubString& x, const char* t);
VMSinline int operator> (const wxSubString& x, const char* t);
VMSinline int operator>=(const wxSubString& x, const char* t);
VMSinline int operator< (const wxSubString& x, const char* t);
VMSinline int operator<=(const wxSubString& x, const char* t);


// status reports, needed before defining other things

inline unsigned int wxString::Length() const {  return rep->len; }
inline int         wxString::Empty() const { return rep->len == 0; }
inline const char* wxString::Chars() const { return &(rep->s[0]); }
// Compatibility with wxWindows
inline char* wxString::GetData() { return (char *)&(rep->s[0]); }
inline int         wxString::Allocation() const { return rep->sz; }
inline void        wxString::Alloc(int newsize) { rep = Sresize(rep, newsize); }

inline unsigned int wxSubString::Length() const { return len; }
inline int         wxSubString::Empty() const { return len == 0; }
inline const char* wxSubString::Chars() const { return &(S.rep->s[pos]); }

// constructors

inline wxString::wxString() 
  : rep(&_nilStrRep) {}
inline wxString::wxString(const wxString& x) 
  : rep(Scopy(0, x.rep)) {}
inline wxString::wxString(const char* t) 
  : rep(Salloc(0, t, -1, -1)) {}
inline wxString::wxString(const char* t, int tlen)
  : rep(Salloc(0, t, tlen, tlen)) {}
inline wxString::wxString(const wxSubString& y)
  : rep(Salloc(0, y.Chars(), y.Length(), y.Length())) {}
inline wxString::wxString(char c) 
  : rep(Salloc(0, &c, 1, 1)) {}

inline wxString::~wxString() { if (rep != &_nilStrRep) delete rep; }

inline wxSubString::wxSubString(const wxSubString& x)
  :S(x.S), pos(x.pos), len(x.len) {}
inline wxSubString::wxSubString(wxString& x, int first, int l)
  :S(x), pos((unsigned short) first), len((unsigned short) l) {}

inline wxSubString::~wxSubString() {}

// assignment

inline wxString& wxString::operator =  (const wxString& y)
{ 
  rep = Scopy(rep, y.rep);
  return *this;
}

inline wxString& wxString::operator=(const char* t)
{
  rep = Salloc(rep, t, -1, -1);
  return *this;
}

inline wxString& wxString::operator=(const wxSubString&  y)
{
  rep = Salloc(rep, y.Chars(), y.Length(), y.Length());
  return *this;
}

inline wxString& wxString::operator=(char c)
{
  rep = Salloc(rep, &c, 1, 1);
  return *this;
}


inline wxSubString& wxSubString::operator = (const char* ys)
{
  assign(0, ys);
  return *this;
}

inline wxSubString& wxSubString::operator = (char ch)
{
  assign(0, &ch, 1);
  return *this;
}

inline wxSubString& wxSubString::operator = (const wxString& y)
{
  assign(y.rep, y.Chars(), y.Length());
  return *this;
}

inline wxSubString& wxSubString::operator = (const wxSubString& y)
{
  assign(y.S.rep, y.Chars(), y.Length());
  return *this;
}

// Zillions of cats...

inline void Cat(const wxString& x, const wxString& y, wxString& r)
{
  r.rep = Scat(r.rep, x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const wxString& x, const wxSubString& y, wxString& r)
{
  r.rep = Scat(r.rep, x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const wxString& x, const char* y, wxString& r)
{
  r.rep = Scat(r.rep, x.Chars(), x.Length(), y, -1);
}

inline void Cat(const wxString& x, char y, wxString& r)
{
  r.rep = Scat(r.rep, x.Chars(), x.Length(), &y, 1);
}

inline void Cat(const wxSubString& x, const wxString& y, wxString& r)
{
  r.rep = Scat(r.rep, x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const wxSubString& x, const wxSubString& y, wxString& r)
{
  r.rep = Scat(r.rep, x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const wxSubString& x, const char* y, wxString& r)
{
  r.rep = Scat(r.rep, x.Chars(), x.Length(), y, -1);
}

inline void Cat(const wxSubString& x, char y, wxString& r)
{
  r.rep = Scat(r.rep, x.Chars(), x.Length(), &y, 1);
}

inline void Cat(const char* x, const wxString& y, wxString& r)
{
  r.rep = Scat(r.rep, x, -1, y.Chars(), y.Length());
}

inline void Cat(const char* x, const wxSubString& y, wxString& r)
{
  r.rep = Scat(r.rep, x, -1, y.Chars(), y.Length());
}

inline void Cat(const char* x, const char* y, wxString& r)
{
  r.rep = Scat(r.rep, x, -1, y, -1);
}

inline void Cat(const char* x, char y, wxString& r)
{
  r.rep = Scat(r.rep, x, -1, &y, 1);
}

inline void Cat(const wxString& a, const wxString& x, const wxString& y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const wxString& a, const wxString& x, const wxSubString& y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const wxString& a, const wxString& x, const char* y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x.Chars(), x.Length(), y, -1);
}

inline void Cat(const wxString& a, const wxString& x, char y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x.Chars(), x.Length(), &y, 1);
}

inline void Cat(const wxString& a, const wxSubString& x, const wxString& y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const wxString& a, const wxSubString& x, const wxSubString& y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const wxString& a, const wxSubString& x, const char* y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x.Chars(), x.Length(), y, -1);
}

inline void Cat(const wxString& a, const wxSubString& x, char y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x.Chars(), x.Length(), &y, 1);
}

inline void Cat(const wxString& a, const char* x, const wxString& y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x, -1, y.Chars(), y.Length());
}

inline void Cat(const wxString& a, const char* x, const wxSubString& y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x, -1, y.Chars(), y.Length());
}

inline void Cat(const wxString& a, const char* x, const char* y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x, -1, y, -1);
}

inline void Cat(const wxString& a, const char* x, char y, wxString& r)
{
  r.rep = Scat(r.rep, a.Chars(), a.Length(), x, -1, &y, 1);
}


inline void Cat(const char* a, const wxString& x, const wxString& y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const char* a, const wxString& x, const wxSubString& y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const char* a, const wxString& x, const char* y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x.Chars(), x.Length(), y, -1);
}

inline void Cat(const char* a, const wxString& x, char y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x.Chars(), x.Length(), &y, 1);
}

inline void Cat(const char* a, const wxSubString& x, const wxString& y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const char* a, const wxSubString& x, const wxSubString& y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x.Chars(), x.Length(), y.Chars(), y.Length());
}

inline void Cat(const char* a, const wxSubString& x, const char* y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x.Chars(), x.Length(), y, -1);
}

inline void Cat(const char* a, const wxSubString& x, char y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x.Chars(), x.Length(), &y, 1);
}

inline void Cat(const char* a, const char* x, const wxString& y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, y.Chars(), y.Length());
}

inline void Cat(const char* a, const char* x, const wxSubString& y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, y.Chars(), y.Length());
}

inline void Cat(const char* a, const char* x, const char* y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, y, -1);
}

inline void Cat(const char* a, const char* x, char y, wxString& r)
{
  r.rep = Scat(r.rep, a, -1, x, -1, &y, 1);
}


// operator versions

inline wxString& wxString::operator +=(const wxString& y)
{
  Cat(*this, y, *this);
  return *this;
}

inline wxString& wxString::operator +=(const wxSubString& y)
{
  Cat(*this, y, *this);
  return *this;
}

inline wxString& wxString::operator += (const char* y)
{
  Cat(*this, y, *this);
  return *this;
}

inline wxString& wxString:: operator +=(char y)
{
  Cat(*this, y, *this);
  return *this;
}

// constructive concatenation

#if defined(__GNUG__) && !defined(_G_NO_NRV)

inline wxString operator + (const wxString& x, const wxString& y) return r;
{
  Cat(x, y, r);
}

inline wxString operator + (const wxString& x, const wxSubString& y) return r;
{
  Cat(x, y, r);
}

inline wxString operator + (const wxString& x, const char* y) return r;
{
  Cat(x, y, r);
}

inline wxString operator + (const wxString& x, char y) return r;
{
  Cat(x, y, r);
}

inline wxString operator + (const wxSubString& x, const wxString& y) return r;
{
  Cat(x, y, r);
}

inline wxString operator + (const wxSubString& x, const wxSubString& y) return r;
{
  Cat(x, y, r);
}

inline wxString operator + (const wxSubString& x, const char* y) return r;
{
  Cat(x, y, r);
}

inline wxString operator + (const wxSubString& x, char y) return r;
{
  Cat(x, y, r);
}

inline wxString operator + (const char* x, const wxString& y) return r;
{
  Cat(x, y, r);
}

inline wxString operator + (const char* x, const wxSubString& y) return r;
{
  Cat(x, y, r);
}

inline wxString Reverse(const wxString& x) return r;
{
  r.rep = Sreverse(x.rep, r.rep);
}

inline wxString Upcase(const wxString& x) return r;
{
  r.rep = Supcase(x.rep, r.rep);
}

inline wxString Downcase(const wxString& x) return r;
{
  r.rep = Sdowncase(x.rep, r.rep);
}

inline wxString Capitalize(const wxString& x) return r;
{
  r.rep = Scapitalize(x.rep, r.rep);
}

#else /* NO_NRV */

inline wxString operator + (const wxString& x, const wxString& y)
{
  wxString r;  Cat(x, y, r);  return r;
}

inline wxString operator + (const wxString& x, const wxSubString& y) 
{
  wxString r; Cat(x, y, r); return r;
}

inline wxString operator + (const wxString& x, const char* y) 
{
  wxString r; Cat(x, y, r); return r;
}

inline wxString operator + (const wxString& x, char y) 
{
  wxString r; Cat(x, y, r); return r;
}

inline wxString operator + (const wxSubString& x, const wxString& y) 
{
  wxString r; Cat(x, y, r); return r;
}

inline wxString operator + (const wxSubString& x, const wxSubString& y) 
{
  wxString r; Cat(x, y, r); return r;
}

inline wxString operator + (const wxSubString& x, const char* y) 
{
  wxString r; Cat(x, y, r); return r;
}

inline wxString operator + (const wxSubString& x, char y) 
{
  wxString r; Cat(x, y, r); return r;
}

inline wxString operator + (const char* x, const wxString& y) 
{
  wxString r; Cat(x, y, r); return r;
}

inline wxString operator + (const char* x, const wxSubString& y) 
{
  wxString r; Cat(x, y, r); return r;
}

inline wxString Reverse(const wxString& x) 
{
  wxString r; r.rep = Sreverse(x.rep, r.rep); return r;
}

inline wxString Upcase(const wxString& x) 
{
  wxString r; r.rep = Supcase(x.rep, r.rep); return r;
}

inline wxString Downcase(const wxString& x) 
{
  wxString r; r.rep = Sdowncase(x.rep, r.rep); return r;
}

inline wxString Capitalize(const wxString& x) 
{
  wxString r; r.rep = Scapitalize(x.rep, r.rep); return r;
}

#endif

// Prepend

inline wxString& wxString::Prepend(const wxString& y)
{
  rep = Sprepend(rep, y.Chars(), y.Length());
  return(*this);
}

inline wxString& wxString::Prepend(const char* y)
{
  rep = Sprepend(rep, y, -1); 
  return(*this);
}

inline wxString& wxString::Prepend(char y)
{
  rep = Sprepend(rep, &y, 1); 
  return(*this);
}

inline wxString& wxString::Prepend(const wxSubString& y)
{
  rep = Sprepend(rep, y.Chars(), y.Length());
  return(*this);
}

// misc transformations


inline void wxString::Reverse()
{
  rep = Sreverse(rep, rep);
}


inline void wxString::Upcase()
{
  rep = Supcase(rep, rep);
}


inline void wxString::Downcase()
{
  rep = Sdowncase(rep, rep);
}


inline void wxString::Capitalize()
{
  rep = Scapitalize(rep, rep);
}

// element extraction

inline char&  wxString::operator [] (int i) 
{ 
  if (((unsigned)i) >= Length()) Error("invalid index");
  return rep->s[i];
}

inline char  wxString::Elem (int i) const
{ 
  if (((unsigned)i) >= Length()) Error("invalid index");
  return rep->s[i];
}

inline char  wxString::Firstchar() const
{ 
  return Elem(0);
}

inline char  wxString::Lastchar() const
{ 
  return Elem(Length() - 1);
}

// searching

inline int wxString::Index(char c, int startpos) const
{
  return Search(startpos, Length(), c);
}

inline int wxString::Index(const char* t, int startpos) const
{
  return Search(startpos, Length(), t);
}

inline int wxString::Index(const wxString& y, int startpos) const
{   
  return Search(startpos, Length(), y.Chars(), y.Length());
}

inline int wxString::Index(const wxSubString& y, int startpos) const
{   
  return Search(startpos, Length(), y.Chars(), y.Length());
}

inline int wxString::Index(const wxRegex& r, int startpos) const
{
  int unused;  return r.Search(Chars(), Length(), unused, startpos);
}

inline int wxString::Contains(char c) const
{
  return Search(0, Length(), c) >= 0;
}

inline int wxString::Contains(const char* t) const
{   
  return Search(0, Length(), t) >= 0;
}

inline int wxString::Contains(const wxString& y) const
{   
  return Search(0, Length(), y.Chars(), y.Length()) >= 0;
}

inline int wxString::Contains(const wxSubString& y) const
{   
  return Search(0, Length(), y.Chars(), y.Length()) >= 0;
}

inline int wxString::Contains(char c, int p) const
{
  return Match(p, Length(), 0, &c, 1) >= 0;
}

inline int wxString::Contains(const char* t, int p) const
{
  return Match(p, Length(), 0, t) >= 0;
}

inline int wxString::Contains(const wxString& y, int p) const
{
  return Match(p, Length(), 0, y.Chars(), y.Length()) >= 0;
}

inline int wxString::Contains(const wxSubString& y, int p) const
{
  return Match(p, Length(), 0, y.Chars(), y.Length()) >= 0;
}

inline int wxString::Contains(const wxRegex& r) const
{
  int unused;  return r.Search(Chars(), Length(), unused, 0) >= 0;
}

inline int wxString::Contains(const wxRegex& r, int p) const
{
  return r.Match(Chars(), Length(), p) >= 0;
}


inline int wxString::Matches(const wxSubString& y, int p) const
{
  return Match(p, Length(), 1, y.Chars(), y.Length()) >= 0;
}

inline int wxString::Matches(const wxString& y, int p) const
{
  return Match(p, Length(), 1, y.Chars(), y.Length()) >= 0;
}

inline int wxString::Matches(const char* t, int p) const
{
  return Match(p, Length(), 1, t) >= 0;
}

inline int wxString::Matches(char c, int p) const
{
  return Match(p, Length(), 1, &c, 1) >= 0;
}

inline int wxString::Matches(const wxRegex& r, int p) const
{
  int l = (p < 0)? -p : Length() - p;
  return r.Match(Chars(), Length(), p) == l;
}


inline int wxSubString::Contains(const char* t) const
{   
  return S.Search(pos, pos+len, t) >= 0;
}

inline int wxSubString::Contains(const wxString& y) const
{   
  return S.Search(pos, pos+len, y.Chars(), y.Length()) >= 0;
}

inline int wxSubString::Contains(const wxSubString&  y) const
{   
  return S.Search(pos, pos+len, y.Chars(), y.Length()) >= 0;
}

inline int wxSubString::Contains(char c) const
{
  return S.Search(pos, pos+len, c) >= 0;
}

inline int wxSubString::Contains(const wxRegex& r) const
{
  int unused;  return r.Search(Chars(), len, unused, 0) >= 0;
}

inline int wxSubString::Matches(const wxRegex& r) const
{
  return r.Match(Chars(), len, 0) == (int)len;
}


inline int wxString::GSub(const wxString& pat, const wxString& r)
{
  return _gsub(pat.Chars(), pat.Length(), r.Chars(), r.Length());
}

inline int wxString::GSub(const wxSubString&  pat, const wxString& r)
{
  return _gsub(pat.Chars(), pat.Length(), r.Chars(), r.Length());
}

inline int wxString::GSub(const wxRegex& pat, const wxString& r)
{
  return _gsub(pat, r.Chars(), r.Length());
}

inline int wxString::GSub(const char* pat, const wxString& r)
{
  return _gsub(pat, -1, r.Chars(), r.Length());
}

inline int wxString::GSub(const char* pat, const char* r)
{
  return _gsub(pat, -1, r, -1);
}



inline  ostream& operator<<(ostream& s, const wxString& x)
{
   s << x.Chars(); return s;
}

// a zillion comparison operators

inline int operator==(const wxString& x, const wxString& y) 
{
  return Compare(x, y) == 0; 
}

inline int operator!=(const wxString& x, const wxString& y)
{
  return Compare(x, y) != 0; 
}

inline int operator>(const wxString& x, const wxString& y)
{
  return Compare(x, y) > 0; 
}

inline int operator>=(const wxString& x, const wxString& y)
{
  return Compare(x, y) >= 0; 
}

inline int operator<(const wxString& x, const wxString& y)
{
  return Compare(x, y) < 0; 
}

inline int operator<=(const wxString& x, const wxString& y)
{
  return Compare(x, y) <= 0; 
}

inline int operator==(const wxString& x, const wxSubString&  y) 
{
  return Compare(x, y) == 0; 
}

inline int operator!=(const wxString& x, const wxSubString&  y)
{
  return Compare(x, y) != 0; 
}

inline int operator>(const wxString& x, const wxSubString&  y)      
{
  return Compare(x, y) > 0; 
}

inline int operator>=(const wxString& x, const wxSubString&  y)
{
  return Compare(x, y) >= 0; 
}

inline int operator<(const wxString& x, const wxSubString&  y) 
{
  return Compare(x, y) < 0; 
}

inline int operator<=(const wxString& x, const wxSubString&  y)
{
  return Compare(x, y) <= 0; 
}

inline int operator==(const wxString& x, const char* t) 
{
  return Compare(x, t) == 0; 
}

inline int operator!=(const wxString& x, const char* t) 
{
  return Compare(x, t) != 0; 
}

inline int operator>(const wxString& x, const char* t)  
{
  return Compare(x, t) > 0; 
}

inline int operator>=(const wxString& x, const char* t) 
{
  return Compare(x, t) >= 0; 
}

inline int operator<(const wxString& x, const char* t)  
{
  return Compare(x, t) < 0; 
}

inline int operator<=(const wxString& x, const char* t) 
{
  return Compare(x, t) <= 0; 
}

inline int operator==(const wxSubString& x, const wxString& y) 
{
  return Compare(y, x) == 0; 
}

inline int operator!=(const wxSubString& x, const wxString& y)
{
  return Compare(y, x) != 0;
}

inline int operator>(const wxSubString& x, const wxString& y)      
{
  return Compare(y, x) < 0;
}

inline int operator>=(const wxSubString& x, const wxString& y)     
{
  return Compare(y, x) <= 0;
}

inline int operator<(const wxSubString& x, const wxString& y)      
{
  return Compare(y, x) > 0;
}

inline int operator<=(const wxSubString& x, const wxString& y)     
{
  return Compare(y, x) >= 0;
}

inline int operator==(const wxSubString& x, const wxSubString&  y) 
{
  return Compare(x, y) == 0; 
}

inline int operator!=(const wxSubString& x, const wxSubString&  y)
{
  return Compare(x, y) != 0;
}

inline int operator>(const wxSubString& x, const wxSubString&  y)      
{
  return Compare(x, y) > 0;
}

inline int operator>=(const wxSubString& x, const wxSubString&  y)
{
  return Compare(x, y) >= 0;
}

inline int operator<(const wxSubString& x, const wxSubString&  y) 
{
  return Compare(x, y) < 0;
}

inline int operator<=(const wxSubString& x, const wxSubString&  y)
{
  return Compare(x, y) <= 0;
}

inline int operator==(const wxSubString& x, const char* t) 
{
  return Compare(x, t) == 0; 
}

inline int operator!=(const wxSubString& x, const char* t) 
{
  return Compare(x, t) != 0;
}

inline int operator>(const wxSubString& x, const char* t)  
{
  return Compare(x, t) > 0; 
}

inline int operator>=(const wxSubString& x, const char* t) 
{
  return Compare(x, t) >= 0; 
}

inline int operator<(const wxSubString& x, const char* t)  
{
  return Compare(x, t) < 0; 
}

inline int operator<=(const wxSubString& x, const char* t) 
{
  return Compare(x, t) <= 0; 
}


// a helper needed by at, before, etc.

inline wxSubString wxString::_substr(int first, int l)
{
  if (first < 0 || (unsigned)(first + l) > Length() )
    return wxSubString(_nilString, 0, 0) ;
  else 
    return wxSubString(*this, first, l);
}

#endif
