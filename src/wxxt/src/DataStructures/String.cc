/*								-*- C++ -*-
 * $Id: String.cc,v 1.1 1996/01/10 14:55:33 markus Exp $
 *
 * Purpose: wxWindows string class (AIAI code)
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#ifdef __GNUG__
#pragma implementation "String.h"
#endif

#define  Uses_wxString
#include "wx.h"

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>

IMPLEMENT_DYNAMIC_CLASS(wxString, wxObject)

//-----------------------------------------------------------------------------
// constructors / destructor
//-----------------------------------------------------------------------------

wxString::wxString()
{
    stringData_ = NULL;
}

wxString::wxString(const wxString& cs)
{
    const char *oldData = cs.GetData();
    if (oldData)
	stringData_ = copystring(oldData);
    else stringData_ = NULL;
}

wxString::wxString(char c)
{                        
    stringData_ = new char[2];
    stringData_[0] = c;
    stringData_[1] = '\0';
}

wxString::wxString(char *s)
{
    if (s)
	stringData_ = copystring(s);
    else
	stringData_ = NULL;
}

wxString::wxString(const char *fmt, ...) // formatted construction
{
    va_list args;
    
    va_start(args,fmt);
    if(fmt==NULL){
	stringData_ = NULL;
    }else{
	char tempString[512];
	vsprintf(tempString,fmt,args);
	stringData_ = copystring(tempString);
    }
    va_end(args);
}

wxString::~wxString(void)
{
    if (stringData_)
	delete[] stringData_;
}

//-----------------------------------------------------------------------------
// constructors / destructor
//-----------------------------------------------------------------------------

int wxString::sprintf(const char *fmt, ...)
{
    va_list args;
    int result;

    va_start(args,fmt);
    wxASSERT(fmt!=NULL,
	     "int wxString::sprintf(const char *fmt, ...): fmt == NULL");
    char tempString[512];
    result = (int)vsprintf(tempString,fmt,args);
    SetData(copystring(tempString));
    va_end(args);
    return(result);
}

//-----------------------------------------------------------------------------
// assignment
//-----------------------------------------------------------------------------

wxString& wxString::operator =(const wxString& cs)
{
    *this = cs.GetData();
    return(*this);
}

wxString& wxString::operator =(const char *cs)
{
    if (stringData_)
	delete[] stringData_;
    const char *oldData = cs;
    if (oldData)
	stringData_ = copystring(oldData);
    else stringData_ = NULL;
    return(*this);
}

wxString& wxString::operator +=(const wxString& cs)
{
    Append(cs);
    return(*this);
}

wxString& wxString::operator +=(const char *cs)
{
    Append(cs);
    return(*this);
}

//-----------------------------------------------------------------------------
// operators
//-----------------------------------------------------------------------------

wxString wxString::operator +(const wxString& cs) const
{
    return(*this + cs.GetData());
}

wxString wxString::operator +(const char *cs) const
{
    const char *s = GetData();
    if (!cs && !s) {
	wxString string;
	return(string);
    } else if (!cs && s) {
	wxString string((char *)s);
	return(string);
    } else if (cs && !s) {
	wxString string((char *)cs);
	return(string);
    } else {
	char *buf = new char[strlen(cs) + strlen(s) + 1];
	strcpy(buf, s);
	strcat(buf, cs);
	wxString string;
	string.SetData(buf);
	return(string);
    }
}

char& wxString::operator [](int i)
{
    assertElement(i);
    return(stringData_[i]);
}

char& wxString::operator ()(size_t i)
{
    return(stringData_[i]);
}

wxString wxString::operator ()(size_t start, size_t len) const
{
    wxString string;
    const char *s = GetData();
    if (!s || len==0)
	return(string);
    size_t mylen = strlen(s);
    size_t to = start+len-1;
    if (start >= mylen)
	return(string);
    if (to >= mylen)
	to = mylen - 1;
    char *buf = new char[to - start + 2];
    size_t i = start;
    for (/*size_t i = start*/; i <= to; i++)
	buf[i-start] = s[i];
    buf[i-start] = 0;
    string.SetData(buf);
    return(string);
}

//-----------------------------------------------------------------------------
// Append / Insert / Prepend
//-----------------------------------------------------------------------------

wxString& wxString::Append(const char* cs)
{
    const char *s = GetData();
    char *buf=NULL;
    if(s == NULL) {
	buf = new char[strlen(cs)+1];
	strcpy(buf,cs);
    } else {
	buf = new char[strlen(cs)+strlen(s)+1];
	strcpy(buf,s);
	strcat(buf,cs);
    }
    SetData(buf);
    return (*this);
}

wxString& wxString::Append(const wxString& cs)
{
    return(Append(cs.GetData()));
}

wxString& wxString::Append(char c, size_t rep)
{
    const char *s = GetData();
    char *buf=NULL;
    if (s == NULL) {
	buf = new char[rep+1];
	char *p=buf;
	for (size_t i=0; i<rep; i++, p++)
	    *p = c;
	*p = '\0';
    } else {                       
	size_t len = strlen(s);
	buf = new char[len+rep+1];
	strcpy(buf,s);
	char *p=buf+len;
	for (size_t i=0; i<rep; i++, p++)
	    *p = c;
	*p = '\0';
    }
    SetData(buf);
    return (*this);
}

wxString& wxString::Insert(size_t pos, const char* cs)
{
    const char *s = GetData();
    size_t len = strlen(s);

    wxASSERT(cs != NULL,
	     "wxString& wxString::Insert(pos, cs): NULL argument");
    wxASSERT(s != NULL,
	     "wxString& wxString::Insert(pos, cs): self is NULL");
    wxASSERT(pos < len,
	     "wxString& wxString::Insert(pos, cs): insert at undefined position");

    char *buf = new char[len+strlen(cs)+1];

    if (pos == 0) { // Insert at beginning
	strcpy(buf,cs);
	strcat(buf,s);
    } else {
	strncpy(buf, s, pos); // copy first part
	strcpy(buf+pos, cs); // copy new part
	strcat(buf, s+pos); // copy rest
    }
    SetData(buf);
    return (*this);
}

wxString& wxString::Insert(size_t pos, const wxString& cs)
{
    return (Insert(pos,cs.GetData()));
}

wxString& wxString::Prepend(const char* cs)
{                      
    wxASSERT(cs!=NULL, "wxString& wxString::Prepend(const char* cs): NULL argument");
    return(Insert(0,cs));
}

wxString& wxString::Prepend(const wxString& cs)
{
    return(Prepend(cs.GetData()));
}

wxString& wxString::Prepend(char c, size_t rep)
{
    // construct repetition of c
    char *temp = new char[rep+1];
    size_t i=0;
    for(/*size_t i=0*/; i<rep; i++) temp[i]=c;
    temp[i]='\0';
    const char *s=GetData();
    if (s == NULL) {
	SetData(temp);
    } else {
	char *buf = new char[strlen(s)+rep+1];
	strcpy(buf,temp);
	strcat(buf,s);
	delete [] temp;
	SetData(buf);
    }
    return(*this);
}

//-----------------------------------------------------------------------------
// String comparisons
//-----------------------------------------------------------------------------

int wxString::CompareTo(const char* cs2, caseCompare cmp) const
{
    const char* cs1 = GetData();
    if (cs2 == NULL) {
	return (cs1 == cs2 ? 0 : 1);
    }
    size_t len = Length();
    size_t i = 0;
    if (cmp == exact) {
	for (; cs2[i]; ++i) {
	    if (i == len)
		return -1;
	    if (cs1[i] != cs2[i])
		return ((cs1[i] > cs2[i]) ? 1 : -1);
	}
    } else {                  // ignore case
	for (; cs2[i]; ++i) {
	    if (i == len)
		return -1;
	    char c1 = (char)wxToUpper((unsigned char)cs1[i]);
	    char c2 = (char)wxToUpper((unsigned char)cs2[i]);
	    if (c1 != c2)
		return ((c1 > c2)? 1 : -1);
	}
    }
    return (i < len) ? 1 : 0;
}

int wxString::CompareTo(const wxString& st, caseCompare cmp) const
{
    return(CompareTo(st.GetData(),cmp));
}

//-----------------------------------------------------------------------------
// SubString
//-----------------------------------------------------------------------------

Bool wxString::Contains(const char* pat, caseCompare cmp) const
{
    wxASSERT(pat != NULL,
	     "Bool wxString::Contains(const char* pat, caseCompare cmp) const: NULL argument");
    return(Index(pat,0,cmp)!=NO_POS);
}

Bool wxString::Contains(const wxString& pat, caseCompare cmp) const
{
    return(Index(pat,0,cmp)!=NO_POS);
}

size_t wxString::First(char c) const
{
    const char *s = GetData();
    if (s != NULL) {
	const char *pos = strchr(s,c);
	if (pos == NULL) {
	    return (NO_POS);
	} else {
	    return (pos-s);
	}
    }
    return(NO_POS);
}

size_t wxString::First(const char* cs) const
{
    wxASSERT(cs != NULL,
	     "size_t wxString::First(const char* cs) const: NULL argument");
    const char *s = GetData();
    if (s != NULL && strlen(cs) != 0) {
	const char *pos = strstr(s, cs);
	if (pos != NULL)
	    return(pos-s);
    }
    return(NO_POS);
}

size_t wxString::First(const wxString& cs) const
{
    return (First(cs.GetData()));
}

size_t wxString::Last(const char* cs) const
{
    wxASSERT(cs != NULL,
	     "size_t wxString::Last(const char* cs) const: NULL argument");
    const char *s = GetData();
    if (s != NULL && strlen((char *)cs) != 0) {
	const char *pos = strstr((char *)s,cs);
	if (pos != NULL)
	    while (TRUE) {
		const char *lastPos = pos;
		pos = strstr(pos+1, cs);
		if (pos == NULL)
		    return (lastPos-s);
	    }
    }
    return (NO_POS);
}

size_t wxString::Last(const wxString& cs) const
{
    return (Last(cs.GetData()));
}

size_t wxString::Last(char c) const
{
    const char *s = GetData();
    if (s != NULL) {
	const char *pos = strrchr(s,c);
	if (pos != NULL)
	    return(pos-s);
    }
    return(NO_POS);
}

wxString wxString::SubString(const char* pat, size_t start) const 
{
    size_t i = Index(pat,start);
    if (i == NO_POS) {
	wxString string;
	return(string);
    }
    return ((*this)(i,strlen(pat)));

}

wxString wxString::SubString(const wxString& pat, size_t start) const
{
    return(this->SubString(pat.GetData(),start));
}

wxString wxString::SubString(size_t from, size_t to)
{
    wxString string;

    if (stringData_) {
	size_t len = strlen(stringData_);
	if (from < len) {
	    if (to >= len) to = len - 1;
	    char *buf = new char[to - from + 2];
	    int i = from;
	    for (/*int i = from*/; i <= (long)to; i++)
		buf[i] = stringData_[i];
	    buf[i] = '\0';
	    string.SetData(buf);
	}
    }
    return string;
}

//-----------------------------------------------------------------------------
// Pattern Matching
//-----------------------------------------------------------------------------

// compare memory case-insensitive
static int mem_insensitive_equal(const char* p, const char* q, size_t n)
{
    while (n--) {
	if (wxToUpper((unsigned char)*p) != wxToUpper((unsigned char)*q))
	    return FALSE;
	p++; q++;
    }
    return TRUE;
}

size_t wxString::Index(const char* pattern, size_t startIndex, caseCompare cmp) const
{
    wxASSERT(pattern!=NULL,
	     "size_t wxString::Index(pattern, startIndex, cmp) const: NULL argument");
    size_t slen = Length();
    size_t plen = strlen(pattern);
    if (slen < startIndex + plen)
	return NO_POS;
    if (plen == 0)
	return startIndex;
    slen -= startIndex + plen;
    const char* sp = GetData() + startIndex;
    if (cmp == exact) {
	char first = *pattern;
	for (size_t i = 0; i <= slen; ++i)
	    if (sp[i] == first && memcmp(sp+i+1, pattern+1, plen-1) == 0)
		return i + startIndex;
    } else {
	int first = wxToUpper((unsigned char) *pattern);
	for (size_t i = 0; i <= slen; ++i)
	    if (wxToUpper((unsigned char) sp[i]) == first &&
		mem_insensitive_equal(sp+i+1, pattern+1, plen-1)) return i + startIndex;
    }
    return NO_POS;
}
  
size_t wxString::Index(const wxString& cs, size_t i, caseCompare cmp) const
{
    return (Index(cs.GetData(),i,cmp));
}

//-----------------------------------------------------------------------------
// ctype equivalents
//-----------------------------------------------------------------------------
              
Bool wxString::IsAscii() const
{
    const char *s = GetData();
    wxASSERT(s != NULL,"Bool wxString::IsAscii() const: self is NULL");
    while (*s) {
	if (!isascii(*s))
	    return(FALSE);
	s++;
    }
    return(TRUE);
}

Bool wxString::IsWord() const
{
    const char *s = GetData();
    wxASSERT(s != NULL,"Bool wxString::IsWord() const: self is NULL");
    while(*s){
	if (!isalpha(*s))
	    return(FALSE);
	s++;
    }
    return(TRUE);
}

Bool wxString::IsNumber() const
{
    const char *s = GetData();
    wxASSERT(s != NULL,"Bool wxString::IsNumber() const: self is NULL");
    while(*s){
	if (!isdigit(*s))
	    return(FALSE);
	s++;
    }
    return(TRUE);
}
  
size_t wxString::Length() const
{
    const char *s = GetData();
    wxASSERT(s != NULL, "size_t wxString::Length() const: self is NULL");
    return(strlen(s));
}

wxString wxString::Copy() const
{
    wxString string(*this);
    return(string);
}

wxString& wxString::Remove(size_t pos)
{
    if (pos < Length()) {
	char *s = stringData_;
	s[pos] = '\0';
	stringData_ = new char[pos+1];
	strcpy(stringData_,s);
	delete[] s;
    }
    return(*this);
}

wxString& wxString::Remove(size_t pos, size_t n)       // Remove n chars starting at pos
{
    const char *s = GetData();
    wxASSERT(s != NULL,"Bool wxString::Remove: self is NULL");
    size_t len=Length();
    if (pos < len) {
	if (pos+n >= len) {
	    Remove(pos);
	} else {
	    char *buf = new char[len-n];
	    char *p=buf;
	    for(size_t i=0; i<pos; i++)
		*p++ = *s++;
	    s += n;
	    while(*s)
		*p++ = *s++;
	    *p = '\0';
	    SetData(buf);
	}
    }
    return(*this);
}

wxString& wxString::Replace(size_t pos, size_t n, const char *cs)
{
    size_t len=Length();
    if (pos < len) {
	char *p = stringData_+pos;
	while (*p && n>0) { // replace as much as possible
	    *p++ = *cs++;
	    n--;
	}
    }
    return(*this);
}

wxString& wxString::Replace(size_t pos, size_t n, const wxString& cs)
{
    Replace(pos,n,cs.GetData());
    return(*this);
}

// Return a substring of self stripped at beginning and/or end
wxString wxString::Strip(stripType stype, char c) const
{
    size_t start = 0;     // Index of first character
    size_t end = Length();    // One beyond last character
    const char* direct = GetData();  

    wxASSERT((int)stype != 0,
	     "wxString  wxString::Strip(stripType stype, char c) const: no strip-type defined");
    if (stype & leading)
	while (start < end && direct[start] == c)
	    ++start;
    if (stype & trailing)
	while (start < end && direct[end-1] == c)
	    --end;
    if (end == start) { // make the null substring
	wxString string;
	return(string);
    }
    return ((*this)(start, end-start));
}

void wxString::UpperCase(void)
{
    char *s = stringData_; // no const possible
    if (s != NULL) {
	int len = strlen(s);
	for (int i = 0; i < len; i++)
	    s[i] = (char)wxToUpper(s[i]);
    }
}

void wxString::LowerCase(void)
{
    char *s = stringData_; // no const possible
    if (s != NULL) {
	int len = strlen(s);
	for (int i = 0; i < len; i++)
	    s[i] = (char)wxToLower(s[i]);
    }
}

wxString& wxString::RemoveLast(void)
{
    char *s = stringData_; // no const possible
    if (s != NULL) {
	s[strlen(s)-1] = '\0';
    }
    return(*this);
}

//-----------------------------------------------------------------------------
// private functions
//-----------------------------------------------------------------------------

void wxString::SetData(char *s)
{
    if (stringData_)
	delete[] stringData_;
    stringData_ = s;
}      

void wxString::assertElement(size_t WXUNUSED(i)) const // Index in range
{
//  const char *s = GetData();
//  wxASSERT(s != NULL,"void wxString::assertElement(size_t i) const: self is NULL");
//  wxASSERT(i<strlen(s),"void wxString::assertElement(size_t i) const: index out of range");
}
