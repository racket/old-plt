/*
 * File:	wx_frac.cc
 * Purpose:	wxFraction class, from NIHCL
 * Author:	Julian Smart, after K. E. Gorlen
 * Created:	1995
 * Updated:	
 * Copyright:	Portions (c) 1995, AIAI, University of Edinburgh
 */

/*
Function:
        
Implements a fraction as two integers, the numerator and the
denominator.  WARNING -- this implementation is not suitable for serious
numeric applications.  Reference: Knuth, "The Art of Computer
Programming", Vol. 2, Section 4.5.
* 
*/

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "common.h"

#endif

#include "wx_frac.h"

#include <stdlib.h>
#include <iostream.h>
#include <math.h>

IMPLEMENT_DYNAMIC_CLASS(wxFraction, wxObject)

long wxFraction::gcd(long uu, long vv)
/* gcd -- binary greatest common divisor algorithm

Algorithm B, p. 321.

*/
{
	register long u=(long)labs(uu), v=(long)labs(vv);
        register int k=0;
	register long t;
        if (u == 0) return v;
        if (v == 0) return u;
        while ((u&1) == 0 && (v&1) == 0) { u>>=1; v>>=1; k++; }
        if (u&1) { t = -v; goto B4; }
        else t = u;
        do {
B3:             t/=2;
B4:             if ((t&1) == 0) goto B3;
                if (t>0) u = t;
                else v = -t;
                t = u-v;
        } while (t != 0);
        return u<<k;
}

void wxFraction::Init(long num, long den)
/*
        Construct a wxFraction from the specified numerator and denominator.
*/
{
        n = num; d = den;
//        if (d == 0) setError(NIHCL_ZERODEN,DEFAULT,this,num,den);
        if (n == 0) { d = 1; return; }
        if (d < 0) { n = -n; d = -d; }
        Reduce();
}

inline char* gcvt(double val, int dig, char* buf)
{
        sprintf(buf,"%*.g",dig,val);
        return buf;
}

wxFraction::wxFraction(double x)
/*
        Construct a wxFraction from a double.
*/
{
//        char buf[20];
        int exp;
        double m = frexp(x,&exp);
        register int k;
        if (exp>=0) {
//		if (exp > (sizeof(long)*8-2)) setError(NIHCL_FCTNOV,DEFAULT,this,gcvt(x,20,buf));
		k = (sizeof(long)*8-2);
        }
        else {
		k = exp+(sizeof(long)*8-2);
//                if (k < 0) setError(NIHCL_FCTNUN,DEFAULT,this,gcvt(x,20,buf));
        }
	n = (long)(m*(1L<<k));
	d = 1L << (k-exp);
        Reduce();
}

void wxFraction::ParseFraction(istream& strm)
/*
        Read a wxFraction from an istream.
*/
{
        n = 0; d = 1;
        strm >> n;
        char slash;
        strm >> slash;
        if (slash == '/') {
                strm >> d;
                Reduce();
        }
        else strm.putback(slash);
}

wxFraction::wxFraction(istream& strm)       { ParseFraction(strm); }

void wxFraction::Reduce()
/*
        Reduce a wxFraction to lowest terms by dividing the numerator and
        denominator by their gcd.
*/
{
	register long d1 = gcd(n,d);
        if (d1 == 1) return;
        n /= d1; d /= d1;
}

wxFraction operator+(const wxFraction& u, const wxFraction& v)
{
	register long d1 = wxFraction::gcd(u.d,v.d);
        if (d1 == 1) return wxFraction(u.n*v.d+u.d*v.n, u.d*v.d, 0);
	register long t = u.n*(v.d/d1) + v.n*(u.d/d1);
	register long d2 = wxFraction::gcd(t,d1);
        return wxFraction(t/d2, (u.d/d1)*(v.d/d2), 0);
}

wxFraction operator-(const wxFraction& u, const wxFraction& v)
{
	register long d1 = wxFraction::gcd(u.d,v.d);
        if (d1 == 1) return wxFraction(u.n*v.d-u.d*v.n, u.d*v.d, 0);
	register long t = u.n*(v.d/d1) - v.n*(u.d/d1);
	register long d2 = wxFraction::gcd(t,d1);
        return wxFraction(t/d2, (u.d/d1)*(v.d/d2), 0);
}

Bool operator<(const wxFraction& u, const wxFraction& v)
{
	register long d1 = wxFraction::gcd(u.d,v.d);
        if (d1 == 1) return u.n*v.d < u.d*v.n;
        return u.n*(v.d/d1) < v.n*(u.d/d1);
}

Bool operator<=(const wxFraction& u, const wxFraction& v)
{
        if (u==v) return TRUE;
        return u<v;
}

wxFraction operator*(const wxFraction& u, const wxFraction& v)
{
	register long d1 = wxFraction::gcd(u.n, v.d);
	register long d2 = wxFraction::gcd(u.d, v.n);
        return wxFraction((u.n/d1)*(v.n/d2), (u.d/d2)*(v.d/d1), 0);
}

wxFraction operator/(const wxFraction& u, const wxFraction& v)
{
        if (v.n < 0) return u*wxFraction(-v.d,-v.n, 0);
        return u*wxFraction(v.d,v.n,0);
}

Bool wxFraction::Between(const wxFraction& theMin, const wxFraction& theMax) const
/*
        Return TRUE if this wxFraction is <= to max and >= to min.
*/
{
        return *this >= theMin && *this <= theMax;
}

wxFraction wxFraction::Max(const wxFraction& f) const
{
        if (f < *this) return *this;
        else return f;
}

wxFraction wxFraction::Min(const wxFraction& f) const
{
        if (f > *this) return *this;
        else return f;
}

/*
unsigned wxFraction::hash() const { return n^d; }

Bool wxFraction::isEqual(const Object& ob) const
{
        return ob.isSpecies(classDesc) && *this==castdown(ob);
}

const Class* wxFraction::species() const { return &classDesc; }

int wxFraction::compare(const Object& ob) const
{
        assertArgSpecies(ob,classDesc,"compare");
        const wxFraction& f = castdown(ob);
        if (*this == f) return 0;
        if (*this < f) return -1;
        return 1;
}

void wxFraction::deepenShallowCopy()      {}

void wxFraction::printOn(ostream& strm) const
{
        if (n == 0 || d == 1) strm << n;
        else strm << n << '/' << d;
}

void wxFraction::scanFrom(istream& strm)  { ParseFraction(strm); }

wxFraction::wxFraction(OIOin& strm)
        : BASE(strm)
{
        strm >> n >> d;
}

void wxFraction::storer(OIOout& strm) const
{
        BASE::storer(strm);
        strm << n << d;
}

wxFraction::wxFraction(OIOifd& fd)
        : BASE(fd)
{
        fd >> n >> d;
}

void wxFraction::storer(OIOofd& fd) const
{
        BASE::storer(fd);
        fd << n << d;
}
*/

