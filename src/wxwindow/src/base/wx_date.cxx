/*
 * File:	wx_date.cc
 * Purpose:	Date class
 * Authors:	See below
 * Created:	1995
 * Updated:	
 * Copyright:	(c) 1995, AIAI, University of Edinburgh + contributors
 * Acknowledgements:
 *    Originally inspired by Steve Marcus (CIS 72007,1233)  6/16/91
 *    Enhanced by Eric Simon (CIS 70540,1522)               6/29/91
 *    Further Enhanced by Chris Hill (CIS 72030,2606)       7/11/91
 *    Still Further Enhanced by Hill & Simon  v3.10         8/05/91
 *    Version 4 by Charles D. Price                         6/27/92
 *    Integrated into wxWindows by Julian Smart             9th July 1995
 */

#if defined(_MSC_VER)
# include "wx.h"
#else

#include "wx_types.h"

#endif

#include "wx_date.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <iostream.h>

#include <time.h>
#include <string.h>

IMPLEMENT_DYNAMIC_CLASS(wxDate, wxObject)

#define ABBR_LENGTH 3

static const char *dayname[] = {"Sunday","Monday","Tuesday","Wednesday",
	   "Thursday","Friday","Saturday"} ;

static const char *mname[] = {"January","February","March","April","May",
	   "June","July","August","September","October","November","December"};

static int GauDays[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

////////////////////////////////////////////////////////////
// Constructors
////////////////////////////////////////////////////////////

wxDate::wxDate()
{
  __type = wxTYPE_DATE;
  DisplayFormat=wxMDY;
  DisplayOptions='\0';
  month = day = year = day_of_week = 0;
  julian = 0;
}

wxDate::wxDate (const long j) : julian(j)
{
  __type = wxTYPE_DATE;
  DisplayFormat=wxMDY;
  DisplayOptions='\0';
  julian_to_mdy ();
}

wxDate::wxDate (const int m, const int d, const int y) : month(m), day(d), year(y)
{
  __type = wxTYPE_DATE;
  DisplayFormat=wxMDY;
  DisplayOptions='\0';
  mdy_to_julian ();
}

wxDate::wxDate (char *dat)
{
  __type = wxTYPE_DATE;

  DisplayFormat=wxMDY;
  DisplayOptions='\0';
  if (strcmp(dat, "TODAY") == 0 || strcmp(dat, "today") == 0)
  {
    // Sets the current date
    Set();
  }
  else
  {
    char buf[100];
    strcpy(buf, dat);

    char *token = strtok(buf,"/-");
    month = atoi(token);
    day   = atoi(strtok(NULL,"/-"));
    year  = atoi(strtok(NULL," "));
  }

  mdy_to_julian ();
}

/*
wxDate::wxDate (const _dosdate_t &ds)
{
  DisplayFormat=wxMDY;
  DisplayOptions='\0';
  month = ds.month;
  day   = ds.day;
  year  = ds.year;
  mdy_to_julian ();
}
*/

wxDate::wxDate (const wxDate &dt)
{
  __type = wxTYPE_DATE;
  DisplayFormat=dt.DisplayFormat;
  DisplayOptions=dt.DisplayOptions;
  month = dt.month;
  day   = dt.day;
  year  = dt.year;
  mdy_to_julian ();
}

void wxDate::operator = (const wxDate &dt)
{
  DisplayFormat=dt.DisplayFormat;
  DisplayOptions=dt.DisplayOptions;
  month = dt.month;
  day   = dt.day;
  year  = dt.year;
  mdy_to_julian ();
}

void wxDate::operator = (const char *dat)
{
  DisplayFormat=wxMDY;
  DisplayOptions='\0';
  if (strcmp(dat, "TODAY") == 0 || strcmp(dat, "today") == 0)
  {
    // Sets the current date
    Set();
  }
  else
  {
    char buf[100];
    strcpy(buf, dat);

    char *token = strtok(buf,"/-");
    month = atoi(token);
    day   = atoi(strtok(NULL,"/-"));
    year  = atoi(strtok(NULL," "));
  }

  mdy_to_julian ();
}

//////////////////////////////////////////////////////////////
// Conversion operations
//////////////////////////////////////////////////////////////

wxDate::operator char *( void )
{
/*
	static char *buf = new char[11];
	if (day==0 || month==0 || year==0)
		strcpy(buf,"bad date");
	else
		sprintf(buf,"%1d/%1d/%4d",month,day,year);
	return buf;
*/
  return FormatDate();
}

//////////////////////////////////////////////////////////////
// Date Arithmetic
//////////////////////////////////////////////////////////////

wxDate wxDate::operator + (const long i)
{
/*
	wxDate *dp = new wxDate(julian + i);
	return *dp;
*/
  wxDate dp(julian + i);
  return dp;
}

wxDate wxDate::operator + (const int i)
{
/*
    wxDate *dp = new wxDate(julian + (long)i);
	return *dp;
*/
  wxDate dp(julian + (long)i);
  return dp;
}

wxDate wxDate::operator - (const long i)
{
/*
	wxDate *dp = new wxDate (julian - i);
	return *dp;

*/
  wxDate dp(julian - i);
  return dp;
}

wxDate wxDate::operator - (const int i)
{
/*
    wxDate *dp = new wxDate (julian - (long)i);
	return *dp;
*/
  wxDate dp(julian - (long)i);
  return dp;
}

long wxDate::operator - (const wxDate &dt)
{
	return ( julian - dt.julian );
}

wxDate &wxDate::operator += (const long i)
{
	 julian += i;
     julian_to_mdy();
	 return *this;
}

wxDate &wxDate::operator -= (const long i)
{
	 julian -= i;
     julian_to_mdy();
	 return *this;
}

wxDate &wxDate::operator ++()
{
	julian++;
    julian_to_mdy();
	return *this;
}

wxDate &wxDate::operator ++(int)
{
	julian++;
    julian_to_mdy();
	return *this;
}

wxDate &wxDate::operator --()
{
	julian--;
    julian_to_mdy();
	return *this;
}

wxDate &wxDate::operator --(int)
{
	julian--;
    julian_to_mdy();
	return *this;
}

//////////////////////////////////////////////////////////////
// Date comparison
//////////////////////////////////////////////////////////////

int operator <  (const wxDate &dt1, const wxDate &dt2)
{
	return ( dt1.julian < dt2.julian );
}

int operator <= (const wxDate &dt1, const wxDate &dt2)
{
	return ( (dt1.julian == dt2.julian) || (dt1.julian < dt2.julian) );
}

int operator >  (const wxDate &dt1, const wxDate &dt2)
{
	return ( dt1.julian > dt2.julian );
}

int operator >= (const wxDate &dt1, const wxDate &dt2)
{
	return ( (dt1.julian == dt2.julian) || (dt1.julian > dt2.julian) );
}

int operator == (const wxDate &dt1, const wxDate &dt2)
{
	return ( dt1.julian == dt2.julian );
}

int operator != (const wxDate &dt1, const wxDate &dt2)
{
	return ( dt1.julian != dt2.julian );
}

////////////////////////////////////////////////////////////////
// Ostream operations
////////////////////////////////////////////////////////////////

ostream &operator << (ostream &os, const wxDate &dt)
{
	return os << dt.FormatDate();
}

/*
ostream &operator << (ostream &os, const _dosdate_t &dt)
{
    return os << (int)dt.month << "/" << (int)dt.day << "/" << dt.year;
}
*/

//////////////////////////////////////////////////////////////
// Conversion routines
//////////////////////////////////////////////////////////////

void wxDate::julian_to_wday (void)
{
	day_of_week = (int) ((julian + 2) % 7 + 1);
}

void wxDate::julian_to_mdy ()
{
	long a,b,c,d,e,z,alpha;
	z = julian+1;
	// dealing with Gregorian calendar reform
    if (z < 2299161L)
       a = z;
    else {
       alpha = (long) ((z-1867216.25) / 36524.25);
       a = z + 1 + alpha - alpha/4;
    }
	b = ( a > 1721423 ? a + 1524 : a + 1158 );
	c = (long) ((b - 122.1) / 365.25);
	d = (long) (365.25 * c);
	e = (long) ((b - d) / 30.6001);
    day = (int)(b - d - (long)(30.6001 * e));
    month = (int)((e < 13.5) ? e - 1 : e - 13);
    year = (int)((month > 2.5 ) ? (c - 4716) : c - 4715);
	julian_to_wday ();
}

void wxDate::mdy_to_julian (void)
{
	int a,b=0;
	int work_month=month, work_day=day, work_year=year;
	// correct for negative year
    if (work_year < 0)
		work_year++;
	if (work_month <= 2)
		{ work_year--; work_month +=12; }

	// deal with Gregorian calendar
	if (work_year*10000. + work_month*100. + work_day >= 15821015.)
		{
        a = (int)(work_year/100.);
		b = 2 - a + a/4;
		}
	julian = (long) (365.25*work_year) +
			 (long) (30.6001 * (work_month+1))  +  work_day + 1720994L + b;
	julian_to_wday ();
}

////////////////////////////////////////////////////////////////
// Format routine
////////////////////////////////////////////////////////////////

char *wxDate::FormatDate (const int type) const
{
  int actualType = type;
  if (actualType == -1)
    actualType = DisplayFormat;

    static char buf[40];

    memset( buf, '\0', sizeof(buf) );
	switch ( actualType )
	{
		case wxDAY:
			if ( (day_of_week < 1) || (day_of_week > 7) )
				strcpy(buf,"invalid day");
			else
				strncpy( buf, dayname[day_of_week-1],
					(DisplayOptions & wxDATE_ABBR) ? ABBR_LENGTH : 9);
			return buf;

		case wxMONTH:
			if ( (month < 1) || (month > 12) )
				strcpy(buf,"invalid month");
			else
				strncpy( buf, mname[month-1],
					(DisplayOptions & wxDATE_ABBR) ? ABBR_LENGTH : 9);
			return buf;

		case wxFULL:
			if ( (month < 1) || (month > 12) || (day_of_week < 0) ||
				 (day_of_week > 7) )
			{
				strcpy(buf,"invalid date");
				return buf;
			}
			strncpy( buf, dayname[day_of_week-1],
				(DisplayOptions & wxDATE_ABBR) ? ABBR_LENGTH : 9);
			strcat( buf, ", ");
			strncat( buf, mname[month-1],
				(DisplayOptions & wxDATE_ABBR) ? ABBR_LENGTH : 9);
			strcat( buf, " ");
			sprintf( buf+strlen(buf), "%d, %d", day, abs(year) );
			if (year < 0)
				strcat(buf," B.C.");
			return buf;

		case wxEUROPEAN:
			if ( (month < 1) || (month > 12) || (day_of_week < 0) ||
				 (day_of_week > 7) )
			{
				strcpy(buf,"invalid date");
				return buf;
			}
			sprintf(buf,"%d ",	day);
			strncat(buf, mname[month-1],
				(DisplayOptions & wxDATE_ABBR) ? ABBR_LENGTH : 9);
			sprintf( buf+strlen(buf), " %d", abs(year) );
			if (year < 0)
				strcat(buf," B.C.");
			return buf;

		case wxMDY:
		default:
			if (day==0 || month==0 || year==0)
				strcpy(buf,"invalid date");
			else
				sprintf( buf+strlen(buf), "%1d/%1d/%02d", month, day,
					(DisplayOptions & wxNO_CENTURY) && (abs(year) > 1899)
					? (abs(year) - (abs(year) / 100 * 100))
					: (abs(year))  );
			return buf;
	}
  return NULL;
}

void wxDate::SetFormat( const int format )
{
	DisplayFormat = format;
}

int wxDate::SetOption( const int option, const Bool action )
{
	switch ( option )
	{
		case wxNO_CENTURY:
			if ( action )
				DisplayOptions |= wxNO_CENTURY;
			else
			{
				DisplayOptions &= (~wxNO_CENTURY);
			}
			return 1;
		case wxDATE_ABBR:
			if ( action )
				DisplayOptions |= wxDATE_ABBR;
			else
			{
				DisplayOptions &= (~wxDATE_ABBR);
			}
			return 1;
		default:
			return 0;
	}
  return 0;
}

///////////////////////////////////////////////////////////////
//  Miscellaneous Routines
///////////////////////////////////////////////////////////////

long wxDate::GetJulianDate( void ) const
{
	return julian;
}

int wxDate::GetDayOfYear( void ) const
{
	wxDate temp( 1, 1, year );

	return (int) (julian - temp.julian + 1);
}


Bool wxDate::IsLeapYear( void ) const
{
	return  ( (year >= 1582) ?
			  (year % 4 == 0  &&  year % 100 != 0  ||  year % 400 == 0 ):
			  (year % 4 == 0) );
}

/*
_dosdate_t wxDate::eom( void ) const
{
    static _dosdate_t eom_temp;
	wxDate tempdate( (month % 12) + 1, 1, year);
	if (month == 12)
		tempdate.year++;
	tempdate--;

    eom_temp.year  = tempdate.year;
    eom_temp.month = tempdate.month;
    eom_temp.day   = tempdate.day;

	return eom_temp;
}

_dosdate_t wxDate::getDate( void ) const
{
    static _dosdate_t getDate_temp;
    getDate_temp.year  = year;
    getDate_temp.month = month;
    getDate_temp.day   = day;
	return getDate_temp;
}
*/

// Version 4.0 Extension to Public Interface - CDP

PUBLIC wxDate &
wxDate::Set()
{
//#ifdef wx_msw
#if 0
    struct _dosdate_t sDate;
    _dos_getdate(&sDate);

    month = sDate.month;
    day   = sDate.day;
    year  = sDate.year;

    mdy_to_julian();
#else
    time_t now = time(NULL);
    struct tm *localTime = localtime(&now);

    month = localTime->tm_mon + 1;
    day = localTime->tm_mday;
    year = localTime->tm_year + 1900;

    mdy_to_julian ();
#endif
    return *this;
}

PUBLIC wxDate &
wxDate::Set(
	int	nMonth,
	int	nDay,
	int	nYear)
{
    month = nMonth;
    year  = nYear < 0 ? 9999 : nYear;
    year  = nYear > 9999 ? 0 : nYear;
    day   = nDay < GetDaysInMonth() ? nDay : GetDaysInMonth();

    mdy_to_julian();
	return *this;
}

PUBLIC wxDate &
wxDate::Set(long j)
{
  julian = j;

  julian_to_mdy();
  return *this;
}


PUBLIC int
wxDate::GetDaysInMonth()
{
    return GauDays[month-1] + (month==2 && IsLeapYear());
}

PUBLIC int
wxDate::GetFirstDayOfMonth() const
{
    return wxDate(month, 1, year).GetDayOfWeek();
}

PUBLIC int
wxDate::GetDay() const
{
    return day;
}

PUBLIC int
wxDate::GetDayOfWeek() const
{
    return day_of_week;
}

PUBLIC int
wxDate::GetYear() const
{
    return year;
}

PUBLIC int
wxDate::GetMonth() const
{
    return month;
}

PUBLIC wxDate &
wxDate::AddWeeks(
    int nCount)
{
    Set(julian + (long)nCount*7);
    return *this;
}


PUBLIC wxDate &
wxDate::AddMonths(
    int nCount)
{
    month += nCount;

    if (month < 1) {
        month = 12;
        year--;
	}

    if (month > 12) {
        month = 1;
        year++;
	}
    mdy_to_julian();
    return *this;
}

PUBLIC wxDate &
wxDate::AddYears(
    int nCount)
{
    year += nCount;
    mdy_to_julian();
    return *this;
}

PUBLIC int
wxDate::GetWeekOfMonth()
{
  // Abs day includes the days from previous month that fills up
  // the begin. of the week.
  int nAbsDay = day + GetFirstDayOfMonth()-1;
  return (nAbsDay-GetDayOfWeek())/7 + 1;
}

PUBLIC int
wxDate::GetWeekOfYear()
{
    wxDate   doTemp(1, 1, year);
    return (int)(((julian - doTemp.julian+1)/7) + 1);
}

PUBLIC wxDate
wxDate::GetMonthStart()
{
    return(wxDate(month, 1, year));
}

PUBLIC wxDate
wxDate::GetMonthEnd()
{
    return(wxDate(month+1, 1, year)-1);
}

PUBLIC wxDate
wxDate::GetYearStart()
{
    return(wxDate(1, 1, year));
}

PUBLIC wxDate
wxDate::GetYearEnd()
{
    return(wxDate(1, 1, year+1)-1);
}

PUBLIC char *
wxDate::GetMonthName()
{
    return(FormatDate(wxMONTH));
}

PUBLIC char *
wxDate::GetDayOfWeekName()
{
    return(FormatDate(wxDAY));
}

Bool wxDate::IsBetween(const wxDate& first, const wxDate& second) const
{
  return (julian >= first.julian && julian <= second.julian);
}

// This function is from NIHCL
wxDate wxDate::Previous(const int dayOfWeek) const
{
        int this_day_Of_Week, desired_day_Of_Week;
        long j;

//      Set the desired and current day of week to start at 0 (Monday)
//      and end at 6 (Sunday).

        desired_day_Of_Week = dayOfWeek - 1; // These functions return a value
        this_day_Of_Week    = GetDayOfWeek() - 1;            // from 1-7.  Subtract 1 for 0-6.
        j = julian;

//      Have to determine how many days difference from current day back to
//      desired, if any.  Special calculation under the 'if' statement to
//      effect the wraparound counting from Monday (0) back to Sunday (6).

        if (desired_day_Of_Week > this_day_Of_Week)
                this_day_Of_Week += 7 - desired_day_Of_Week;
        else
                this_day_Of_Week -= desired_day_Of_Week;
        j -= this_day_Of_Week; // Adjust j to set it at the desired day of week.
        return wxDate(j);
}
