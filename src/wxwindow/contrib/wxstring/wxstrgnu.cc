// This file was adapted to usage in non-GNU environments
// (especially under wxWindows) by Stefan Hammes (steve),
// (stefan.hammes@urz.uni-heidelberg.de)
//
// There were also some minor extensions made. They can
// be found in the beginning of the code.
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
//       class in definition. therefore the split/join members are
//       not available under VMS
// Stefan Hammes (steve), 03.12.94
//
// 25.05.95: all member functions capitalized to fit naming convention
//           of wxWindows. added member functions from the simple string
//           class of wxWindows. changed void return types to return
//           a reference to 'this'.

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

/* 
  wxString class implementation
 */

#ifdef __GNUG__
#pragma implementation
#endif

#ifdef wx_msw
// disables warnings about conversion between different integral types and
// '==' : signed/unsigned mismatch
#pragma warning( disable : 4135 4018 )
#endif

#ifndef VMS
// this is included in [.contrib.wxstring]wxstrgnu.h under VMS,
// but in \include\base\wxstrgnu.h under DOS or UNIX
#include "wxregex.h"
#endif
#include "wxstrgnu.h" // changed to wxstrgnu by steve, 1.12.94

//#include <std.h>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <new.h>
//#include <builtin.h>
                                                   
//********************************************************************************
//********************************************************************************
//********************************************************************************
// additions by steve, 1.12.94
#include <stdarg.h>

#define   wxToUpper(C)      (((C) >= 'a' && (C) <= 'z')? (C) - 'a' + 'A': (C))
#define   wxToLower(C)      (((C) >= 'A' && (C) <= 'Z')? (C) - 'A' + 'a': (C))
                   
static const int sprintfBufferSize = 1024;

inline static int slen(const char* t); // inline  strlen (prototype)

#if 0 // do not use anymore: clash with GNU constructor!
// generate a string (sprintf-like)
// used also for normal construction 'wxString a("hello");'
// fmt == NULL is allowed!
wxString::wxString(const char *fmt, ...) // formatted construction
{
    va_list args;

    va_start(args,fmt);
    if(fmt==NULL){
      rep = Salloc(0, fmt, -1, -1);
    }else{
      char *tempString,buf[sprintfBufferSize];
      int allocated=0;
      int len=slen(fmt);
      if(len>sprintfBufferSize/2){
        tempString = new char[len*2]; // for long strings make a big buffer
        allocated = 1;
      }else{
        tempString = buf;
      }
      vsprintf(tempString,fmt,args);
      rep = Salloc(0, tempString, -1, -1);
      if(allocated) delete[] tempString;
    }
    va_end(args);
}
#endif


void wxString::sprintf(const char *fmt, ...)
{
    va_list args;

    va_start(args,fmt);
    char *tempString,buf[sprintfBufferSize];
    int allocated=0;
    int len=slen(fmt);
    if(len>sprintfBufferSize/2){
      tempString = new char[len*2];
      allocated = 1;
    }else{
      tempString = buf;
    }
    vsprintf(tempString,fmt,args);
    rep = Salloc(0, tempString, -1, -1);
    if(allocated) delete[] tempString;
    va_end(args);
}

Bool wxString::IsAscii(void) const
{
  const char *s = Chars();
  while(*s){
    if(!isascii(*s)) return(FALSE);
    s++;
  }
  return(TRUE);
}
  
Bool wxString::IsWord(void) const
{
  const char *s = Chars();
  while(*s){
    if(!isalpha(*s)) return(FALSE);
    s++;
  }
  return(TRUE);
}
  
Bool wxString::IsNumber(void) const
{
  const char *s = Chars();
  while(*s){
    if(!isdigit(*s)) return(FALSE);
    s++;
  }
  return(TRUE);
}

Bool wxString::IsNull(void) const
{
  // we have 2 ways how an null representation is defined!
  // a wxString is only null when it has nothing assigned or
  // has assigned a NULL pointer.
  return(rep==NULL || rep==&_nilStrRep);
}

// Return a substring of self stripped at beginning and/or end
wxSubString wxString::Strip(StripType stype, char c)
{
  int start = 0;     // Index of first character
  int end = Length();    // One beyond last character
  const char* direct = Chars();  

  if (stype & leading)
    while (start < end && direct[start] == c)
      ++start;
  if (stype & trailing)
    while (start < end && direct[end-1] == c)
      --end;
      
  return(_substr(start, end-start));
}

// problem: how to check for EOF?
int Readline(FILE *f, wxString& x, char terminator, int discard)
{
  if (ferror(f)!=0)
    return 0;
  int ch;
  int i = 0;
  x.rep = Sresize(x.rep, 80);
  while ((ch = fgetc(f)) != EOF)
  {
    if (ch != terminator || !discard)
    {
      if (i >= x.rep->sz - 1)
        x.rep = Sresize(x.rep, i+1);
      x.rep->s[i++] = ch;
    }
    if (ch == terminator)
      break;
  }
  x.rep->s[i] = 0;
  x.rep->len = i;
  return i;
}

// addition by steve for non-GNU environments
static void testErrorHandler(const char *className, const char *msg)
{
  cerr << className << ": " << msg << "\n";
}
static void (*lib_error_handler)(const char *, const char *) = testErrorHandler;

// additions for compatibility with simple wxWindows string class

// String comparisons
int wxString::CompareTo(const char* s, CaseCompare cmp) const
{
  if(cmp==ignoreCase){
    return(FCompare(*this,s));
  }else{
    return(Compare(*this,s));
  }
}

int wxString::CompareTo(const wxString& st, CaseCompare cmp) const
{
  if(cmp==ignoreCase){
    return(FCompare(*this,st));
  }else{
    return(Compare(*this,st));
  }
}

// compare memory case-insensitive
static Bool mem_insensitive_equal(const char* p, const char* q, int n)
{
  while (n--){
    if (wxToUpper((unsigned char)*p) != wxToUpper((unsigned char)*q)) return FALSE;
    p++; q++;
  }
  return TRUE;
}

// Pattern Matching:
int wxString::Index(const char* pattern, int startIndex, CaseCompare cmp) const
{
  assert(pattern!=NULL && "int wxString::Index(const char* pattern, int startIndex, CaseCompare cmp) const: NULL argument");
  int slen = Length();
  int plen = strlen(pattern);
  if (slen < startIndex + plen) return NO_POS;
  if (plen == 0) return startIndex;
  slen -= startIndex + plen;
  const char* sp = Chars() + startIndex;
  if (cmp == exact) {
    char first = *pattern;
    for (int i = 0; i <= slen; ++i)
      if (sp[i] == first && memcmp(sp+i+1, pattern+1, plen-1) == 0)
        return i + startIndex;
  } else {
    int first = wxToUpper((unsigned char) *pattern);
    for (int i = 0; i <= slen; ++i)
      if (wxToUpper((unsigned char) sp[i]) == first &&
        mem_insensitive_equal(sp+i+1, pattern+1, plen-1)) return i + startIndex;
  }
  return NO_POS;
}
  
int wxString::Index(const wxString& cs, int i, CaseCompare cmp) const
{
  return(Index(cs.Chars(),i,cmp));
}

Bool wxString::Contains(const char* pat, CaseCompare cmp) const
{
  assert(pat!=NULL&&"Bool wxString::Contains(const char* pat, CaseCompare cmp) const: NULL argument");
  return(Index(pat,0,cmp)!=NO_POS);
}

Bool wxString::Contains(const wxString& pat, CaseCompare cmp) const
{
  return(Index(pat,0,cmp)!=NO_POS);
}

wxString wxString::Copy() const
{
  wxString string(*this);
  return(string);
}

int wxString::First(char c) const
{
  const char *s = Chars();
  if(s == NULL){
    return(NO_POS);
  }else{
    const char *pos = strchr(s,c);
    if(pos==NULL){
      return(NO_POS);
    }else{
      return(pos-s);
    }
  }
}

int wxString::First(const char* cs) const
{
  assert(cs!=NULL&&"int wxString::First(const char* cs) const: NULL argument");
  const char *s = Chars();
  if(s==NULL || strlen(cs)==0){
    return(NO_POS);
  }else{
// PHE
#if defined(__BORLANDC__) && defined(WIN32)
    const char *pos = strstr((char *)s,(char *)cs);
#else
    const char *pos = strstr(s,cs);
#endif
    if(pos==NULL){
      return(NO_POS);
    }else{
      return(pos-s);
    }
  }
}

int wxString::First(const wxString& cs) const
{
  return(First(cs.Chars()));
}

int wxString::Last(const char* cs) const
{
  assert(cs!=NULL&&"int wxString::Last(const char* cs) const: NULL argument");
  const char *s = Chars();
  if(s==NULL || strlen((char *)cs)==0){
    return(NO_POS);
  }else{
    const char *pos = strstr((char *)s,cs);
    if(pos==NULL){ // it's absolutely not there!
      return(NO_POS);
    }else{
      // try to search the 'cs' again
      while(TRUE){
        const char *lastPos = pos; // remember pos
        pos = strstr(pos+1,cs); // search again (after the pattern)
        if(pos==NULL) return(lastPos-s); // this WILL be reached!
      }
    }
  }
}

int wxString::Last(const wxString& cs) const
{
  return(Last(cs.Chars()));
}

int wxString::Last(char c) const
{
  const char *s = Chars();
  if(s == NULL){
    return(NO_POS);
  }else{
    const char *pos = strrchr(s,c);
    if(pos==NULL){
      return(NO_POS);
    }else{
      return(pos-s);
    }
  }
}

wxString& wxString::Insert(int pos, const char* cs)
{
  assert(cs!=NULL&&"wxString& wxString::Insert(int pos, const char* cs): NULL argument");
  const char *s = Chars();
  assert(s != NULL&&"wxString& wxString::Insert(int pos, const char* cs): self is NULL");
  int len = strlen(s);
  assert(pos < len&&"wxString& wxString::Insert(int pos, const char* cs): insert at undefined position");
  char *buf = new char[len+strlen(cs)+1];
  if(pos==0){ // Insert at beginning
    strcpy(buf,cs);
    strcat(buf,s);
  }else{
    strncpy(buf,s,pos); // copy first part
    strcpy(buf+pos,cs); // copy new part
    strcat(buf,s+pos); // copy rest
  }
  *this = buf;
  return(*this);
}

wxString& wxString::Insert(int pos, const wxString& cs)
{
  return(Insert(pos,cs.Chars()));
}

wxString& wxString::Replace(int pos, int n, const char *cs)
{
  char *s = GetData(); // use GetData to have write access
  assert(s != NULL && "Bool wxString::Replace: self is NULL");
  int len=Length();
  if(pos<len){
    char *p = s+pos;
    while(*p && n>0){ // replace as much as possible
      *p++ = *cs++;
      n--;
    }
  }
  return(*this);
}

wxString& wxString::Replace(int pos, int n, const wxString& cs)
{
  Replace(pos,n,cs.Chars());
  return(*this);
}

//********************************************************************************
//********************************************************************************
//********************************************************************************
// standard libg++ code following

void wxString::Error(const char* msg) const
{
  (*lib_error_handler)("wxString", msg);
}

wxString::operator const char*() const
{ 
  return (const char*)Chars();
}

//  globals

wxStrRep  _nilStrRep = { 0, 1, { 0 } }; // nil strings point here
wxString _nilString;               // nil SubStrings point here




/*
 the following inline fcts are specially designed to work
 in support of wxString classes, and are not meant as generic replacements
 for libc "str" functions.

 inline copy fcts -  I like left-to-right from->to arguments.
 all versions assume that `to' argument is non-null

 These are worth doing inline, rather than through calls because,
 via procedural integration, adjacent copy calls can be smushed
 together by the optimizer.
*/

// copy n bytes
inline static void ncopy(const char* from, char* to, int n)
{
  if (from != to) while (--n >= 0) *to++ = *from++;
}

// copy n bytes, null-terminate
inline static void ncopy0(const char* from, char* to, int n)
{
  if (from != to) 
  {
    while (--n >= 0) *to++ = *from++;
    *to = 0;
  }
  else
    to[n] = 0;
}

// copy until null
inline static void scopy(const char* from, char* to)
{
  if (from != 0) while((*to++ = *from++) != 0);
}

// copy right-to-left
inline static void revcopy(const char* from, char* to, short n)
{
  if (from != 0) while (--n >= 0) *to-- = *from--;
}

inline static int slen(const char* t) // inline  strlen
{
  if (t == 0)
    return 0;
  else
  {
    const char* a = t;
    while (*a++ != 0);
    return a - 1 - t;
  }
}

// minimum & maximum representable rep size

#define MAXStrRep_SIZE   ((1 << (sizeof(short) * CHAR_BIT - 1)) - 1)
#define MINStrRep_SIZE   16

#ifndef MALLOC_MIN_OVERHEAD
#define MALLOC_MIN_OVERHEAD  4
#endif

// The basic allocation primitive:
// Always round request to something close to a power of two.
// This ensures a bit of padding, which often means that
// concatenations don't have to realloc. Plus it tends to
// be faster when lots of Strings are created and discarded,
// since just about any version of malloc (op new()) will
// be faster when it can reuse identically-sized chunks

inline static wxStrRep* Snew(int newsiz)
{
  unsigned int siz = sizeof(wxStrRep) + newsiz + MALLOC_MIN_OVERHEAD;
  unsigned int allocsiz = MINStrRep_SIZE;
  while (allocsiz < siz) allocsiz <<= 1;
  allocsiz -= MALLOC_MIN_OVERHEAD;
  if (allocsiz >= MAXStrRep_SIZE)
    (*lib_error_handler)("wxString", "Requested length out of range");
    
  wxStrRep* rep = (wxStrRep *) new char[allocsiz];
  rep->sz = allocsiz - sizeof(wxStrRep);
  return rep;
}

// Do-something-while-allocating routines.

// We live with two ways to signify empty Sreps: either the
// null pointer (0) or a pointer to the nilStrRep.

// We always signify unknown source lengths (usually when fed a char*)
// via len == -1, in which case it is computed.

// allocate, copying src if nonull

wxStrRep* Salloc(wxStrRep* old, const char* src, int srclen, int newlen)
{
// ++++ added by steve, 06.12.94
  // do not allocate something, when passed a char-NULL pointer (without length)
  if(src == 0 && srclen == -1 && newlen == -1){
    if(old == 0 || old == &_nilStrRep) return(old);
    // if there was something allocated, free it
    delete old;
    return(0); // and return null string
  }
// ---- added by steve, 06.12.94
  
  if (old == &_nilStrRep) old = 0;
  if (srclen < 0) srclen = slen(src);
  if (newlen < srclen) newlen = srclen;
  wxStrRep* rep;
  if (old == 0 || newlen > old->sz)
    rep = Snew(newlen);
  else
    rep = old;

  rep->len = newlen;
  ncopy0(src, rep->s, srclen);

  if (old != rep && old != 0) delete old;

  return rep;
}

// reallocate: Given the initial allocation scheme, it will
// generally be faster in the long run to get new space & copy
// than to call realloc

wxStrRep* Sresize(wxStrRep* old, int newlen)
{
  if (old == &_nilStrRep) old = 0;
  wxStrRep* rep;
  if (old == 0)
    rep = Snew(newlen);
  else if (newlen > old->sz)
  {
    rep = Snew(newlen);
    ncopy0(old->s, rep->s, old->len);
    delete old;
  }
  else
    rep = old;

  rep->len = newlen;

  return rep;
}

// like allocate, but we know that src is a wxStrRep

wxStrRep* Scopy(wxStrRep* old, const wxStrRep* s)
{
  if (old == &_nilStrRep) old = 0;
  if (s == &_nilStrRep) s = 0;
  if (old == s) 
    return (old == 0)? &_nilStrRep : old;
  else if (s == 0)
  {
    old->s[0] = 0;
    old->len = 0;
    return old;
  }
  else 
  {
    wxStrRep* rep;
    int newlen = s->len;
    if (old == 0 || newlen > old->sz)
    {
      if (old != 0) delete old;
      rep = Snew(newlen);
    }
    else
      rep = old;
    rep->len = newlen;
    ncopy0(s->s, rep->s, newlen);
    return rep;
  }
}

// allocate & concatenate

wxStrRep* Scat(wxStrRep* old, const char* s, int srclen, const char* t, int tlen)
{
  if (old == &_nilStrRep) old = 0;
  if (srclen < 0) srclen = slen(s);
  if (tlen < 0) tlen = slen(t);
  int newlen = srclen + tlen;
  wxStrRep* rep;

  if (old == 0 || newlen > old->sz || 
      (t >= old->s && t < &(old->s[old->len]))) // beware of aliasing
    rep = Snew(newlen);
  else
    rep = old;

  rep->len = newlen;

  ncopy(s, rep->s, srclen);
  ncopy0(t, &(rep->s[srclen]), tlen);

  if (old != rep && old != 0) delete old;

  return rep;
}

// double-concatenate

wxStrRep* Scat(wxStrRep* old, const char* s, int srclen, const char* t, int tlen,
             const char* u, int ulen)
{
  if (old == &_nilStrRep) old = 0;
  if (srclen < 0) srclen = slen(s);
  if (tlen < 0) tlen = slen(t);
  if (ulen < 0) ulen = slen(u);
  int newlen = srclen + tlen + ulen;
  wxStrRep* rep;
  if (old == 0 || newlen > old->sz || 
      (t >= old->s && t < &(old->s[old->len])) ||
      (u >= old->s && u < &(old->s[old->len])))
    rep = Snew(newlen);
  else
    rep = old;

  rep->len = newlen;

  ncopy(s, rep->s, srclen);
  ncopy(t, &(rep->s[srclen]), tlen);
  ncopy0(u, &(rep->s[srclen+tlen]), ulen);

  if (old != rep && old != 0) delete old;

  return rep;
}

// like Cat, but we know that new stuff goes in the front of existing rep

wxStrRep* Sprepend(wxStrRep* old, const char* t, int tlen)
{
  char* s;
  int srclen;
  if (old == &_nilStrRep || old == 0)
  {
    s = 0; old = 0; srclen = 0;
  }
  else
  {
    s = old->s; srclen = old->len;
  }
  if (tlen < 0) tlen = slen(t);
  int newlen = srclen + tlen;
  wxStrRep* rep;
  if (old == 0 || newlen > old->sz || 
      (t >= old->s && t < &(old->s[old->len])))
    rep = Snew(newlen);
  else
    rep = old;

  rep->len = newlen;

  revcopy(&(s[srclen]), &(rep->s[newlen]), srclen+1);
  ncopy(t, rep->s, tlen);

  if (old != rep && old != 0) delete old;

  return rep;
}


// string compare: first argument is known to be non-null

inline static int scmp(const char* a, const char* b)
{
  if (b == 0)
    return *a != 0;
  else
  {
    signed char diff = 0;
    while ((diff = *a - *b++) == 0 && *a++ != 0);
    return diff;
  }
}


inline static int ncmp(const char* a, int al, const char* b, int bl)
{
  int n = (al <= bl)? al : bl;
  signed char diff;
  while (n-- > 0) if ((diff = *a++ - *b++) != 0) return diff;
  return al - bl;
}

int FCompare(const wxString& x, const wxString& y)
{
  const char* a = x.Chars();
  const char* b = y.Chars();
  int al = x.Length();
  int bl = y.Length();
  int n = (al <= bl)? al : bl;
  signed char diff = 0;
  while (n-- > 0)
  {
    char ac = *a++;
    char bc = *b++;
    if ((diff = ac - bc) != 0)
    {
      if (ac >= 'a' && ac <= 'z')
        ac = ac - 'a' + 'A';
      if (bc >= 'a' && bc <= 'z')
        bc = bc - 'a' + 'A';
      if ((diff = ac - bc) != 0)
        return diff;
    }
  }
  return al - bl;
}

// these are not inline, but pull in the above inlines, so are 
// pretty fast

int Compare(const wxString& x, const char* b)
{
  return scmp(x.Chars(), b);
}

int Compare(const wxString& x, const wxString& y)
{
  return scmp(x.Chars(), y.Chars());
}

int Compare(const wxString& x, const wxSubString& y)
{
  return ncmp(x.Chars(), x.Length(), y.Chars(), y.Length());
}

int Compare(const wxSubString& x, const wxString& y)
{
  return ncmp(x.Chars(), x.Length(), y.Chars(), y.Length());
}

int Compare(const wxSubString& x, const wxSubString& y)
{
  return ncmp(x.Chars(), x.Length(), y.Chars(), y.Length());
}

int Compare(const wxSubString& x, const char* b)
{
  if (b == 0)
    return x.Length();
  else
  {
    const char* a = x.Chars();
    int n = x.Length();
    signed char diff;
    while (n-- > 0) if ((diff = *a++ - *b++) != 0) return diff;
    return (*b == 0) ? 0 : -1;
  }
}

/*
 index fcts
*/

int wxString::Search(int start, int sl, char c) const
{
  const char* s = Chars();
  if (sl > 0)
  {
    if (start >= 0)
    {
      const char* a = &(s[start]);
      const char* lasta = &(s[sl]);
      while (a < lasta) if (*a++ == c) return --a - s;
    }
    else
    {
      const char* a = &(s[sl + start + 1]);
      while (--a >= s) if (*a == c) return a - s;
    }
  }
  return -1;
}

int wxString::Search(int start, int sl, const char* t, int tl) const
{
  const char* s = Chars();
  if (tl < 0) tl = slen(t);
  if (sl > 0 && tl > 0)
  {
    if (start >= 0)
    {
      const char* lasts = &(s[sl - tl]);
      const char* lastt = &(t[tl]);
      const char* p = &(s[start]);

      while (p <= lasts)
      {
        const char* x = p++;
        const char* y = t;
        while (*x++ == *y++) if (y >= lastt) return --p - s;
      }
    }
    else
    {
      const char* firsts = &(s[tl - 1]);
      const char* lastt =  &(t[tl - 1]);
      const char* p = &(s[sl + start + 1]); 

      while (--p >= firsts)
      {
        const char* x = p;
        const char* y = lastt;
        while (*x-- == *y--) if (y < t) return ++x - s;
      }
    }
  }
  return -1;
}

int wxString::Match(int start, int sl, int exact, const char* t, int tl) const
{
  if (tl < 0) tl = slen(t);

  if (start < 0)
  {
    start = sl + start - tl + 1;
    if (start < 0 || (exact && start != 0))
      return -1;
  }
  else if (exact && sl - start != tl)
    return -1;

  if (sl == 0 || tl == 0 || sl - start < tl || start >= sl)
    return -1;

  int n = tl;
  const char* s = &(rep->s[start]);
  while (--n >= 0) if (*s++ != *t++) return -1;
  return tl;
}

void wxSubString::assign(const wxStrRep* ysrc, const char* ys, int ylen)
{
  if (&S == &_nilString) return;

  if (ylen < 0) ylen = slen(ys);
  wxStrRep* targ = S.rep;
  int sl = targ->len - len + ylen;

  if (ysrc == targ || sl >= targ->sz)
  {
    wxStrRep* oldtarg = targ;
    targ = Sresize(0, sl);
    ncopy(oldtarg->s, targ->s, pos);
    ncopy(ys, &(targ->s[pos]), ylen);
    scopy(&(oldtarg->s[pos + len]), &(targ->s[pos + ylen]));
    delete oldtarg;
  }
  else if (len == ylen)
    ncopy(ys, &(targ->s[pos]), len);
  else if (ylen < len)
  {
    ncopy(ys, &(targ->s[pos]), ylen);
    scopy(&(targ->s[pos + len]), &(targ->s[pos + ylen]));
  }
  else
  {
    revcopy(&(targ->s[targ->len]), &(targ->s[sl]), targ->len-pos-len +1);
    ncopy(ys, &(targ->s[pos]), ylen);
  }
  targ->len = sl;
  S.rep = targ;
}



/*
 * substitution
 */


int wxString::_gsub(const char* pat, int pl, const char* r, int rl)
{
  int nmatches = 0;
  if (pl < 0) pl = slen(pat);
  if (rl < 0) rl = slen(r);
  int sl = Length();
  if (sl <= 0 || pl <= 0 || sl < pl)
    return nmatches;
  
  const char* s = Chars();

  // prepare to make new rep
  wxStrRep* nrep = 0;
  int nsz = 0;
  char* x = 0;

  int si = 0;
  int xi = 0;
  int remaining = sl;

  while (remaining >= pl)
  {
    int pos = Search(si, sl, pat, pl);
    if (pos < 0)
      break;
    else
    {
      ++nmatches;
      int mustfit = xi + remaining + rl - pl;
      if (mustfit >= nsz)
      {
        if (nrep != 0) nrep->len = xi;
        nrep = Sresize(nrep, mustfit);
        nsz = nrep->sz;
        x = nrep->s;
      }
      pos -= si;
      ncopy(&(s[si]), &(x[xi]), pos);
      ncopy(r, &(x[xi + pos]), rl);
      si += pos + pl;
      remaining -= pos + pl;
      xi += pos + rl;
    }
  }

  if (nrep == 0)
  {
    if (nmatches == 0)
      return nmatches;
    else
      nrep = Sresize(nrep, xi+remaining);
  }

  ncopy0(&(s[si]), &(x[xi]), remaining);
  nrep->len = xi + remaining;

  if (nrep->len <= rep->sz)   // fit back in if possible
  {
    rep->len = nrep->len;
    ncopy0(nrep->s, rep->s, rep->len);
    delete(nrep);
  }
  else
  {
    delete(rep);
    rep = nrep;
  }
  return nmatches;
}

int wxString::_gsub(const wxRegex& pat, const char* r, int rl)
{
  int nmatches = 0;
  int sl = Length();
  if (sl <= 0)
    return nmatches;

  if (rl < 0) rl = slen(r);

  const char* s = Chars();

  wxStrRep* nrep = 0;
  int nsz = 0;

  char* x = 0;

  int si = 0;
  int xi = 0;
  int remaining = sl;
  int  pos, pl = 0;				  // how long is a regular expression?

  while (remaining > 0)
  {
    pos = pat.Search(s, sl, pl, si); // unlike string search, the pos returned here is absolute
    if (pos < 0 || pl <= 0)
      break;
    else
    {
      ++nmatches;
      int mustfit = xi + remaining + rl - pl;
      if (mustfit >= nsz)
      {
        if (nrep != 0) nrep->len = xi;
        nrep = Sresize(nrep, mustfit);
        x = nrep->s;
        nsz = nrep->sz;
      }
      pos -= si;
      ncopy(&(s[si]), &(x[xi]), pos);
      ncopy(r, &(x[xi + pos]), rl);
      si += pos + pl;
      remaining -= pos + pl;
      xi += pos + rl;
    }
  }

  if (nrep == 0)
  {
    if (nmatches == 0)
      return nmatches;
    else
      nrep = Sresize(nrep, xi+remaining);
  }

  ncopy0(&(s[si]), &(x[xi]), remaining);
  nrep->len = xi + remaining;

  if (nrep->len <= rep->sz)   // fit back in if possible
  {
    rep->len = nrep->len;
    ncopy0(nrep->s, rep->s, rep->len);
    delete(nrep);
  }
  else
  {
    delete(rep);
    rep = nrep;
  }
  return nmatches;
}

/*
 * deletion
 */

wxString& wxString::Del(int pos, int len)
{
  if (pos < 0 || len <= 0 || (unsigned)(pos + len) > Length()) return(*this);
  int nlen = Length() - len;
  int first = pos + len;
  ncopy0(&(rep->s[first]), &(rep->s[pos]), Length() - first);
  rep->len = nlen;
  return(*this);
}

wxString& wxString::Del(const wxRegex& r, int startpos)
{
  int mlen;
  int first = r.Search(Chars(), Length(), mlen, startpos);
  Del(first, mlen);
  return(*this);
}

wxString& wxString::Del(const char* t, int startpos)
{
  int tlen = slen(t);
  int p = Search(startpos, Length(), t, tlen);
  Del(p, tlen);
  return(*this);
}

wxString& wxString::Del(const wxString& y, int startpos)
{
  Del(Search(startpos, Length(), y.Chars(), y.Length()), y.Length());
  return(*this);
}

wxString& wxString::Del(const wxSubString& y, int startpos)
{
  Del(Search(startpos, Length(), y.Chars(), y.Length()), y.Length());
  return(*this);
}

wxString& wxString::Del(char c, int startpos)
{
  Del(Search(startpos, Length(), c), 1);
  return(*this);
}

/*
 * substring extraction
 */


wxSubString wxString::At(int first, int len)
{
  return _substr(first, len);
}

wxSubString wxString::operator() (int first, int len)
{
  return _substr(first, len);
}

wxSubString wxString::Before(int pos)
{
  return _substr(0, pos);
}

wxSubString wxString::Through(int pos)
{
  return _substr(0, pos+1);
}

wxSubString wxString::After(int pos)
{
  return _substr(pos + 1, Length() - (pos + 1));
}

wxSubString wxString::From(int pos)
{
  return _substr(pos, Length() - pos);
}

wxSubString wxString::At(const wxString& y, int startpos)
{
  int first = Search(startpos, Length(), y.Chars(), y.Length());
  return _substr(first,  y.Length());
}

wxSubString wxString::At(const wxSubString& y, int startpos)
{
  int first = Search(startpos, Length(), y.Chars(), y.Length());
  return _substr(first, y.Length());
}

wxSubString wxString::At(const wxRegex& r, int startpos)
{
  int mlen;
  int first = r.Search(Chars(), Length(), mlen, startpos);
  return _substr(first, mlen);
}

wxSubString wxString::At(const char* t, int startpos)
{
  int tlen = slen(t);
  int first = Search(startpos, Length(), t, tlen);
  return _substr(first, tlen);
}

wxSubString wxString::At(char c, int startpos)
{
  int first = Search(startpos, Length(), c);
  return _substr(first, 1);
}

wxSubString wxString::Before(const wxString& y, int startpos)
{
  int last = Search(startpos, Length(), y.Chars(), y.Length());
  return _substr(0, last);
}

wxSubString wxString::Before(const wxSubString& y, int startpos)
{
  int last = Search(startpos, Length(), y.Chars(), y.Length());
  return _substr(0, last);
}

wxSubString wxString::Before(const wxRegex& r, int startpos)
{
  int mlen;
  int first = r.Search(Chars(), Length(), mlen, startpos);
  return _substr(0, first);
}

wxSubString wxString::Before(char c, int startpos)
{
  int last = Search(startpos, Length(), c);
  return _substr(0, last);
}

wxSubString wxString::Before(const char* t, int startpos)
{
  int tlen = slen(t);
  int last = Search(startpos, Length(), t, tlen);
  return _substr(0, last);
}

wxSubString wxString::Through(const wxString& y, int startpos)
{
  int last = Search(startpos, Length(), y.Chars(), y.Length());
  if (last >= 0) last += y.Length();
  return _substr(0, last);
}

wxSubString wxString::Through(const wxSubString& y, int startpos)
{
  int last = Search(startpos, Length(), y.Chars(), y.Length());
  if (last >= 0) last += y.Length();
  return _substr(0, last);
}

wxSubString wxString::Through(const wxRegex& r, int startpos)
{
  int mlen;
  int first = r.Search(Chars(), Length(), mlen, startpos);
  if (first >= 0) first += mlen;
  return _substr(0, first);
}

wxSubString wxString::Through(char c, int startpos)
{
  int last = Search(startpos, Length(), c);
  if (last >= 0) last += 1;
  return _substr(0, last);
}

wxSubString wxString::Through(const char* t, int startpos)
{
  int tlen = slen(t);
  int last = Search(startpos, Length(), t, tlen);
  if (last >= 0) last += tlen;
  return _substr(0, last);
}

wxSubString wxString::After(const wxString& y, int startpos)
{
  int first = Search(startpos, Length(), y.Chars(), y.Length());
  if (first >= 0) first += y.Length();
  return _substr(first, Length() - first);
}

wxSubString wxString::After(const wxSubString& y, int startpos)
{
  int first = Search(startpos, Length(), y.Chars(), y.Length());
  if (first >= 0) first += y.Length();
  return _substr(first, Length() - first);
}

wxSubString wxString::After(char c, int startpos)
{
  int first = Search(startpos, Length(), c);
  if (first >= 0) first += 1;
  return _substr(first, Length() - first);
}

wxSubString wxString::After(const wxRegex& r, int startpos)
{
  int mlen;
  int first = r.Search(Chars(), Length(), mlen, startpos);
  if (first >= 0) first += mlen;
  return _substr(first, Length() - first);
}

wxSubString wxString::After(const char* t, int startpos)
{
  int tlen = slen(t);
  int first = Search(startpos, Length(), t, tlen);
  if (first >= 0) first += tlen;
  return _substr(first, Length() - first);
}

wxSubString wxString::From(const wxString& y, int startpos)
{
  int first = Search(startpos, Length(), y.Chars(), y.Length());
  return _substr(first, Length() - first);
}

wxSubString wxString::From(const wxSubString& y, int startpos)
{
  int first = Search(startpos, Length(), y.Chars(), y.Length());
  return _substr(first, Length() - first);
}

wxSubString wxString::From(const wxRegex& r, int startpos)
{
  int mlen;
  int first = r.Search(Chars(), Length(), mlen, startpos);
  return _substr(first, Length() - first);
}

wxSubString wxString::From(char c, int startpos)
{
  int first = Search(startpos, Length(), c);
  return _substr(first, Length() - first);
}

wxSubString wxString::From(const char* t, int startpos)
{
  int tlen = slen(t);
  int first = Search(startpos, Length(), t, tlen);
  return _substr(first, Length() - first);
}



/*
 * split/join
 */

#ifndef VMS
int Split(const wxString& src, wxString results[], int n, const wxString& sep)
{
  wxString x = src;
  const char* s = x.Chars();
  int sl = x.Length();
  int i = 0;
  int pos = 0;
  while (i < n && pos < sl)
  {
    int p = x.Search(pos, sl, sep.Chars(), sep.Length());
    if (p < 0)
      p = sl;
    results[i].rep = Salloc(results[i].rep, &(s[pos]), p - pos, p - pos);
    i++;
    pos = p + sep.Length();
  }
  return i;
}

int Split(const wxString& src, wxString results[], int n, const wxRegex& r)
{
  wxString x = src;
  const char* s = x.Chars();
  int sl = x.Length();
  int i = 0;
  int pos = 0;
  int p, matchlen;
  while (i < n && pos < sl)
  {
    p = r.Search(s, sl, matchlen, pos);
    if (p < 0)
      p = sl;
    results[i].rep = Salloc(results[i].rep, &(s[pos]), p - pos, p - pos);
    i++;
    pos = p + matchlen;
  }
  return i;
}

#if defined(__GNUG__) && !defined(_G_NO_NRV)

wxString Join(wxString src[], int n, const wxString& separator) return x;
{
  wxString sep = separator;
  int xlen = 0;
  for (int i = 0; i < n; ++i)
    xlen += src[i].Length();
  xlen += (n - 1) * sep.Length();

  x.Alloc(xlen);

  int j = 0;
  
  for (i = 0; i < n - 1; ++i)
  {
    ncopy(src[i].Chars(), &(x.rep->s[j]), src[i].Length());
    j += src[i].Length();
    ncopy(sep.Chars(), &(x.rep->s[j]), sep.Length());
    j += sep.Length();
  }
  ncopy0(src[i].Chars(), &(x.rep->s[j]), src[i].Length());
}

#else

wxString Join(wxString src[], int n, const wxString& separator)
{
  wxString x;
  wxString sep = separator;
  int xlen = 0;
  for (int i = 0; i < n; ++i)
    xlen += src[i].Length();
  xlen += (n - 1) * sep.Length();

  x.Alloc(xlen);

  int j = 0;
  
  for (i = 0; i < n - 1; ++i)
  {
    ncopy(src[i].Chars(), &(x.rep->s[j]), src[i].Length());
    j += src[i].Length();
    ncopy(sep.Chars(), &(x.rep->s[j]), sep.Length());
    j += sep.Length();
  }
  ncopy0(src[i].Chars(), &(x.rep->s[j]), src[i].Length());
  return x;
}

#endif

#endif // ifndef VMS
  
/*
 misc
*/

    
wxStrRep* Sreverse(const wxStrRep* src, wxStrRep* dest)
{
  int n = src->len;
  if (src != dest)
    dest = Salloc(dest, src->s, n, n);
  if (n > 0)
  {
    char* a = dest->s;
    char* b = &(a[n - 1]);
    while (a < b)
    {
      char t = *a;
      *a++ = *b;
      *b-- = t;
    }
  }
  return dest;
}


wxStrRep* Supcase(const wxStrRep* src, wxStrRep* dest)
{
  int n = src->len;
  if (src != dest) dest = Salloc(dest, src->s, n, n);
  char* p = dest->s;
  char* e = &(p[n]);
  for (; p < e; ++p) if (islower(*p)) *p = toupper(*p);
  return dest;
}

wxStrRep* Sdowncase(const wxStrRep* src, wxStrRep* dest)
{
  int n = src->len;
  if (src != dest) dest = Salloc(dest, src->s, n, n);
  char* p = dest->s;
  char* e = &(p[n]);
  for (; p < e; ++p) if (isupper(*p)) *p = tolower(*p);
  return dest;
}

wxStrRep* Scapitalize(const wxStrRep* src, wxStrRep* dest)
{
  int n = src->len;
  if (src != dest) dest = Salloc(dest, src->s, n, n);

  char* p = dest->s;
  char* e = &(p[n]);
  for (; p < e; ++p)
  {
    int at_word;
    if (at_word = islower(*p))
      *p = toupper(*p);
    else 
      at_word = isupper(*p) || isdigit(*p);

    if (at_word)
    {
      while (++p < e)
      {
        if (isupper(*p))
          *p = tolower(*p);
	/* A '\'' does not break a word, so that "Nathan's" stays
	   "Nathan's" rather than turning into "Nathan'S". */
        else if (!islower(*p) && !isdigit(*p) && (*p != '\''))
          break;
      }
    }
  }
  return dest;
}

#if defined(__GNUG__) && !defined(_G_NO_NRV)

wxString Replicate(char c, int n) return w;
{
  w.rep = Sresize(w.rep, n);
  char* p = w.rep->s;
  while (n-- > 0) *p++ = c;
  *p = 0;
}

wxString Replicate(const wxString& y, int n) return w
{
  int len = y.Length();
  w.rep = Sresize(w.rep, n * len);
  char* p = w.rep->s;
  while (n-- > 0)
  {
    ncopy(y.Chars(), p, len);
    p += len;
  }
  *p = 0;
}

wxString CommonPrefix(const wxString& x, const wxString& y, int startpos) return r;
{
  const char* xchars = x.Chars();
  const char* ychars = y.Chars();
  const char* xs = &(xchars[startpos]);
  const char* ss = xs;
  const char* topx = &(xchars[x.Length()]);
  const char* ys = &(ychars[startpos]);
  const char* topy = &(ychars[y.Length()]);
  for (int l = 0; xs < topx && ys < topy && *xs++ == *ys++; ++l);
  r.rep = Salloc(r.rep, ss, l, l);
}

wxString CommonSuffix(const wxString& x, const wxString& y, int startpos) return r;
{
  const char* xchars = x.Chars();
  const char* ychars = y.Chars();
  const char* xs = &(xchars[x.Length() + startpos]);
  const char* botx = xchars;
  const char* ys = &(ychars[y.Length() + startpos]);
  const char* boty = ychars;
  for (int l = 0; xs >= botx && ys >= boty && *xs == *ys ; --xs, --ys, ++l);
  r.rep = Salloc(r.rep, ++xs, l, l);
}

#else

wxString Replicate(char c, int n)
{
  wxString w;
  w.rep = Sresize(w.rep, n);
  char* p = w.rep->s;
  while (n-- > 0) *p++ = c;
  *p = 0;
  return w;
}

wxString Replicate(const wxString& y, int n)
{
  wxString w;
  int len = y.Length();
  w.rep = Sresize(w.rep, n * len);
  char* p = w.rep->s;
  while (n-- > 0)
  {
    ncopy(y.Chars(), p, len);
    p += len;
  }
  *p = 0;
  return w;
}

wxString CommonPrefix(const wxString& x, const wxString& y, int startpos)
{
  wxString r;
  const char* xchars = x.Chars();
  const char* ychars = y.Chars();
  const char* xs = &(xchars[startpos]);
  const char* ss = xs;
  const char* topx = &(xchars[x.Length()]);
  const char* ys = &(ychars[startpos]);
  const char* topy = &(ychars[y.Length()]);
  for (int l = 0; xs < topx && ys < topy && *xs++ == *ys++; ++l);
  r.rep = Salloc(r.rep, ss, l, l);
  return r;
}

wxString CommonSuffix(const wxString& x, const wxString& y, int startpos) 
{
  wxString r;
  const char* xchars = x.Chars();
  const char* ychars = y.Chars();
  const char* xs = &(xchars[x.Length() + startpos]);
  const char* botx = xchars;
  const char* ys = &(ychars[y.Length() + startpos]);
  const char* boty = ychars;
  for (int l = 0; xs >= botx && ys >= boty && *xs == *ys ; --xs, --ys, ++l);
  r.rep = Salloc(r.rep, ++xs, l, l);
  return r;
}

#endif

// IO

istream& operator>>(istream& s, wxString& x)
{
  if (!s.ipfx(0) || (!(s.flags() & ios::skipws) && !ws(s)))
  {
    s.clear(ios::failbit|s.rdstate()); // Redundant if using GNU iostreams.
    return s;
  }
  int ch;
  int i = 0;
  x.rep = Sresize(x.rep, 20);
  register streambuf *sb = s.rdbuf();
  while ((ch = sb->sbumpc()) != EOF)
  {
    if (isspace(ch))
      break;
    if (i >= x.rep->sz - 1)
      x.rep = Sresize(x.rep, i+1);
    x.rep->s[i++] = ch;
  }
  x.rep->s[i] = 0;
  x.rep->len = i;
  int new_state = s.rdstate();
  if (i == 0) new_state |= ios::failbit;
  if (ch == EOF) new_state |= ios::eofbit;
  s.clear(new_state);
  return s;
}

int Readline(istream& s, wxString& x, char terminator, int discard)
{
  if (!s.ipfx(0))
    return 0;
  int ch;
  int i = 0;
  x.rep = Sresize(x.rep, 80);
  register streambuf *sb = s.rdbuf();
  while ((ch = sb->sbumpc()) != EOF)
  {
    if (ch != terminator || !discard)
    {
      if (i >= x.rep->sz - 1)
        x.rep = Sresize(x.rep, i+1);
      x.rep->s[i++] = ch;
    }
    if (ch == terminator)
      break;
  }
  x.rep->s[i] = 0;
  x.rep->len = i;
  if (ch == EOF) s.clear(ios::eofbit|s.rdstate());
  return i;
}


ostream& operator<<(ostream& s, const wxSubString& x)
{ 
  const char* a = x.Chars();
  const char* lasta = &(a[x.Length()]);
  while (a < lasta)
    s.put(*a++);
  return(s);
}

// from John.Willis@FAS.RI.CMU.EDU

int wxString::Freq(const wxSubString& y) const
{
  int found = 0;
  for (unsigned int i = 0; i < Length(); i++) 
    if (Match(i,Length(),0,y.Chars(), y.Length())>= 0) found++;
  return(found);
}

int wxString::Freq(const wxString& y) const
{
  int found = 0;
  for (unsigned int i = 0; i < Length(); i++) 
    if (Match(i,Length(),0,y.Chars(),y.Length()) >= 0) found++;
  return(found);
}

int wxString::Freq(const char* t) const
{
  int found = 0;
  for (unsigned int i = 0; i < Length(); i++) 
    if (Match(i,Length(),0,t) >= 0) found++;
  return(found);
}

int wxString::Freq(char c) const
{
  int found = 0;
  for (unsigned int i = 0; i < Length(); i++) 
    if (Match(i,Length(),0,&c,1) >= 0) found++;
  return(found);
}


int wxString::OK() const
{
  if (rep == 0             // don't have a rep
    || rep->len > rep->sz     // string oustide bounds
    || rep->s[rep->len] != 0)   // not null-terminated
      Error("invariant failure");
  return 1;
}

int wxSubString::OK() const
{
  int v = S != (const char*)0; // have a wxString;
  v &= S.OK();                 // that is legal
  v &= pos + len >= S.rep->len;// pos and len within bounds
  if (!v) S.Error("wxSubString invariant failure");
  return v;
}
