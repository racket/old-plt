/*
 * File:	wx_frac.h
 * Purpose:	wxFraction class, from NIHCL
 * Author:	Julian Smart, after K. E. Gorlen
 * Created:	1995
 * Updated:	
 * Copyright:	Portions (c) 1995, AIAI, University of Edinburgh
 */

#ifndef wx_frach
#define wx_frach

#include "wx_obj.h"

class wxFraction: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxFraction) /* ; MATTHEW: Get rid of semi-colon */
public:
	static long gcd(long uu, long vv);
private:
	long n,d;
	wxFraction(long num, long den, int dum)
        {
          // WHAT DOES THIS MEAN??!!
          n = (dum,num); d = den;
        }
	void Init(long num, long den);
        void ParseFraction(istream&);
        void Reduce();
/*
protected:              // storer() functions for object I/O
        virtual void storer(OIOofd&) const;
        virtual void storer(OIOout&) const;
*/
public:
	wxFraction(int num, long den=1) { Init(num,den); }
	wxFraction(long num =0, long den =1) { Init(num,den); }
        wxFraction(double);
        wxFraction(istream&);
        operator double() const         { return (double)n/d; }
	long GetDenominator() const	{ return d; }
	long GetNumerator() const		{ return n; }
        
        friend wxFraction operator+(const wxFraction&, const wxFraction&);
        friend wxFraction operator-(const wxFraction& u)  { return wxFraction(-u.n,u.d); }
        friend wxFraction operator-(const wxFraction&, const wxFraction&);
        friend wxFraction operator*(const wxFraction&, const wxFraction&);
        friend wxFraction operator/(const wxFraction&, const wxFraction&);
        friend Bool     operator<(const wxFraction& u, const wxFraction& v);
        friend Bool     operator>(const wxFraction& u, const wxFraction& v)         { return v<u; }
        friend Bool     operator<=(const wxFraction& u, const wxFraction& v);
        friend Bool     operator>=(const wxFraction& u, const wxFraction& v)        { return v<=u; }
        friend Bool     operator==(const wxFraction& u, const wxFraction& v)        { return u.n == v.n && u.d == v.d; }
        friend Bool     operator!=(const wxFraction& u, const wxFraction& v)        { return !(u==v); }
        
        void operator+=(const wxFraction& u)      { *this = *this + u; }
        void operator-=(const wxFraction& u)      { *this = *this - u; }
        void operator*=(const wxFraction& u)      { *this = *this * u; }
        void operator/=(const wxFraction& u)      { *this = *this / u; }
        
        Bool Between(const wxFraction& min, const wxFraction& max) const;
        wxFraction Max(const wxFraction&) const;
        wxFraction Min(const wxFraction&) const;
/*        
        virtual int compare(const Object&) const;
        virtual void    deepenShallowCopy();    // {}
        virtual unsigned hash() const;
        virtual Bool isEqual(const Object&) const;
        virtual void printOn(ostream& strm =cout) const;
        virtual void scanFrom(istream& strm);
*/
};

#endif
