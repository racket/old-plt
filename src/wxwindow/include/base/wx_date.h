/*
 * File:	wx_date.h
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

#ifndef DATECLS_H
#define DATECLS_H

#include "wx_obj.h"

#define PUBLIC           // just a couple of friendly reminders!
#define MEMBER

enum wxdate_format_type {wxMDY, wxDAY, wxMONTH, wxFULL, wxEUROPEAN};

// const enum {wxOFF, wxON};

#define wxNO_CENTURY  0x02
#define wxDATE_ABBR   0x04

class wxDate: public wxObject
{
  DECLARE_DYNAMIC_CLASS(wxDate)
 protected:
   unsigned long julian; // see julDate();  days since 1/1/4713 B.C.
   int year;             // see NYear4()
   int month;            // see NMonth()
   int day;              // see Day()
   int day_of_week;      // see NDOW();  1 = Sunday, ... 7 = Saturday

 private:
//		static int  		 DisplayFormat;
//		static unsigned char DisplayOptions;
  int  		 DisplayFormat;
  unsigned char DisplayOptions;

  void julian_to_mdy ();         // convert julian day to mdy
  void julian_to_wday ();        // convert julian day to day_of_week
  void mdy_to_julian ();         // convert mdy to julian day

 public:
  wxDate ();
  wxDate (const long j);
  wxDate (const int m, const int d, const int y);
  wxDate (      char *dat);

//  wxDate (const _dosdate_t &ds);
  wxDate (const wxDate &dt);

  operator char *   (void);
  void operator =   (const wxDate& date);
  void operator =   (const char *date);

  wxDate operator +  (const long i);
  wxDate operator +  (const int  i);

  wxDate operator -  (const long i);
  wxDate operator -  (const int  i);

  long operator -  (const wxDate &dt);

  wxDate &operator += (const long i);
  wxDate &operator -= (const long i);

  wxDate &operator ++ ();     // Prefix increment
  wxDate &operator ++ (int);  // Postfix increment
  wxDate &operator -- ();     // Prefix decrement
  wxDate &operator -- (int);  // Postfix decrement

  friend int operator <  (const wxDate &dt1, const wxDate &dt2);
  friend int operator <= (const wxDate &dt1, const wxDate &dt2);
  friend int operator >  (const wxDate &dt1, const wxDate &dt2);
  friend int operator >= (const wxDate &dt1, const wxDate &dt2);
  friend int operator == (const wxDate &dt1, const wxDate &dt2);
  friend int operator != (const wxDate &dt1, const wxDate &dt2);

  friend ostream &operator << (ostream &os, const wxDate &dt);
/*
  friend ostream &operator << (ostream &os, const _dosdate_t &dt);
*/

  char *FormatDate 	   (const int type=-1) const;
  void  SetFormat (const int format);
  int   SetOption (const int option, const Bool enable=TRUE);

  long  GetJulianDate() const;  // returns julian date
  int   GetDayOfYear()  const;  // returns relative date since Jan. 1
  Bool  IsLeapYear()    const;  // returns TRUE if leap year, FALSE if not

/*
  // note that the next functions return a date struct as defined in
  // dos.h (distinct from the wxDate class)
  _dosdate_t  eom()       const;  // returns last day of month in object
  _dosdate_t  getDate()   const;  // returns a date structure
*/

  // Version 4.0 Extension to Public Interface - CDP
  
  // These 'Set's modify the date object and actually SET it
  // They all return a reference to self (*this)

  wxDate &Set();            // Sets to current system date
  wxDate &Set(long lJulian);
  wxDate &Set(int nMonth, int nDay, int nYear);

  wxDate &AddWeeks(int nCount = 1);  // 
  wxDate &AddMonths(int nCount = 1); // May also pass neg# to decrement
  wxDate &AddYears(int nCount = 1);  //

  int   GetDay() const;      // Numeric Day of date object
  int   GetDaysInMonth();    // Number of days in month (1..31)
  int   GetFirstDayOfMonth() const; // First Day Of Month  (1..7)

  char *GetDayOfWeekName();       // Character Day Of Week ('Sunday'..'Saturday')
  int   GetDayOfWeek() const;     // (1..7)

  int   GetWeekOfMonth();            // Numeric Week Of Month  (1..6)
  int   GetWeekOfYear();            // Numeric Week Of Year   (1..52)

  char *GetMonthName();            // Character Month name
  int   GetMonth() const;          // Month Number (1..12)
  wxDate  GetMonthStart();         // First Date Of Month
  wxDate  GetMonthEnd();           // Last Date Of Month

  int   GetYear() const;           // eg. 1992
  wxDate  GetYearStart();          // First Date Of Year
  wxDate  GetYearEnd();            // Last Date Of Year

  Bool IsBetween(const wxDate& first, const wxDate& second) const;

  wxDate Previous(const int dayOfWeek) const;
};

#endif
