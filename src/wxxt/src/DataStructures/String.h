/*								-*- C++ -*-
 * $Id: String.h,v 1.1 1996/01/10 14:55:34 markus Exp $
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

#ifndef String_h
#define String_h

#ifdef __GNUG__
#pragma interface
#endif

#define NO_POS ((size_t)(-1)) // undefined position

class wxString : public wxObject {
DECLARE_DYNAMIC_CLASS(wxString)
private:
    char *stringData_;
    void assertElement(size_t) const;    // Index in range
public:
    void SetData(char *s);
    enum stripType {leading = 0x1, trailing = 0x2, both = 0x3};
    enum caseCompare {exact, ignoreCase};

    wxString();
    wxString(char *s);
    wxString(const wxString& s);
    wxString(char);
    wxString(const char *fmt, ...);
    ~wxString();

    // formatted construction
    int sprintf(const char *fmt, ...);
  
    // Type conversion:
    char* Data()          { return stringData_; }
    operator char*()      { return stringData_; }
    char* GetData() const { return stringData_; }

    wxString Copy() const;

    // comparison
    int  CompareTo(const char* cs, caseCompare cmp = exact) const;
    int  CompareTo(const wxString& st, caseCompare cmp = exact) const;
    Bool operator ==(char *s2) const           { return(CompareTo(s2)==0); }
    Bool operator ==(const wxString& s2) const { return(CompareTo(s2)==0); }
    Bool operator !=(char  *s) const           { return(CompareTo(s)!=0);  }
    Bool operator !=(const wxString& s) const  { return(CompareTo(s)!=0);  }
    Bool operator <(const wxString& s) const   { return(CompareTo(s)<0);   }
    Bool operator <(const char  *s) const      { return(CompareTo(s)<0);   }
    Bool operator <=(const wxString& s) const  { return(CompareTo(s)<=0);  }
    Bool operator <=(const char  *s) const     { return(CompareTo(s)<=0);  }
    Bool operator >(const wxString& s) const   { return(CompareTo(s)>0);   }
    Bool operator >(const char  *s) const      { return(CompareTo(s)>0);   }
    Bool operator >=(const wxString& s) const  { return(CompareTo(s)>=0);  }
    Bool operator >=(const char  *s) const     { return(CompareTo(s)>=0);  }
    
    // Assignment:
    wxString& operator=(const char*);
    wxString& operator=(const wxString&);
    wxString& operator+=(const char*);
    wxString& operator+=(const wxString& s);

    // Indexing operators:
    char&    operator[](int);
    char&    operator()(size_t);
    wxString operator()(size_t start, size_t len) const;
    wxString SubString(const char* pat, size_t start=0) const;     
    wxString SubString(const wxString& pat, size_t start=0) const;     
    wxString SubString(size_t from, size_t to);
  
    // Non-static member functions:
    wxString operator +(const wxString& s) const;
    wxString operator +(const char *s) const;

    wxString& Append(const char* cs);
    wxString& Append(const wxString& s);
    wxString& Append(char c, size_t rep=1);

    Bool Contains(const char* pat, caseCompare cmp = exact) const;
    Bool Contains(const wxString& pat, caseCompare cmp = exact) const;

    size_t First(char c) const;
    size_t First(const char* cs) const;
    size_t First(const wxString& cs) const;
  
    size_t Index(const char* pat, size_t i=0, caseCompare cmp = exact) const;
    size_t Index(const wxString& s, size_t i=0, caseCompare cmp = exact) const;
              
    wxString& Insert(size_t pos, const char*);
    wxString& Insert(size_t pos, const wxString&);
  
    Bool IsAscii() const;
    Bool IsNumber() const;
    Bool IsWord() const;
    Bool IsNull() const { return(stringData_ == NULL); }

    size_t Last(char c) const;
    size_t Last(const char* cs) const;
    size_t Last(const wxString& cs) const;

    size_t Length() const;
  
    wxString& Prepend(const char*);
    wxString& Prepend(const wxString& s);
    wxString& Prepend(char c, size_t rep=1);
  
    wxString& Remove(size_t pos);
    wxString& Remove(size_t pos, size_t n);
    wxString& RemoveLast(void);

    wxString& Replace(size_t pos, size_t n, const char*);
    wxString& Replace(size_t pos, size_t n, const wxString&);

    wxString Strip(stripType s=trailing, char c=' ') const;

    void LowerCase();
    void UpperCase();
};

#endif // String_h
