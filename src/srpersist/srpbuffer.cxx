// srpbuffer.cxx -- buffer reading procedures

#ifdef WIN32
#include <windows.h>
#endif

#include <sqltypes.h>
#include <sql.h>

#include "escheme.h"
#include "srpersist.h"
#include "srptypes.h"
#include "srpbuffer.h"

#include <stdarg.h>

/*

Scheme_Object *scheme_make_list(Scheme_Object *obj,...) {
  va_list ap;
  int count;
  Scheme_Object *ps[512];
  Scheme_Object *retval;

  count = 0;
  ps[0] = obj;

  va_start(ap,obj);

  while (ps[count] != NULL) {
    ps[++count] = va_arg(ap,Scheme_Object *);
  }

  va_end(ap);

  retval = scheme_null;

  for (--count; count >= 0; count--) {
    retval = scheme_make_pair(ps[count],retval);
  } 

  return retval;
}

*/

Scheme_Object *readCharBuffer(char *buffer,long numElts) {
  return scheme_make_sized_string(buffer,numElts,TRUE);
}

Scheme_Object *readLongBuffer(long *buffer,long numElts) {
  long i;
  Scheme_Object *retval;
  long *p;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(scheme_make_integer_value(*p),retval);
  }

  return retval;
}

Scheme_Object *readULongBuffer(unsigned long *buffer,long numElts) {
  long i;
  Scheme_Object *retval;
  unsigned long *p;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(scheme_make_integer_value_from_unsigned(*p),retval);
  }

  return retval;
}

Scheme_Object *readShortBuffer(short *buffer,long numElts) {
  long i;
  Scheme_Object *retval;
  short *p;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(scheme_make_integer_value((long)*p),retval);
  }

  return retval;
}

Scheme_Object *readUShortBuffer(unsigned short *buffer,long numElts) {
  long i;
  Scheme_Object *retval;
  unsigned short *p;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(scheme_make_integer_value((long)*p),retval);
  }

  return retval;
}

Scheme_Object *readFloatBuffer(float *buffer,long numElts) {
  long i;
  Scheme_Object *retval;
  float *p;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(scheme_make_double((double)*p),retval);
  }

  return retval;
}

Scheme_Object *readDoubleBuffer(double *buffer,long numElts) {
  long i;
  Scheme_Object *retval;
  double *p;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(scheme_make_double(*p),retval);
  }

  return retval;
}

Scheme_Object *makeNumericList(SQL_NUMERIC_STRUCT *obj) {
  Scheme_Object *precision;
  Scheme_Object *scale;
  Scheme_Object *sign;
  Scheme_Object *val;
  Scheme_Object *retval;

  precision = scheme_make_character(obj->precision);
  scale = scheme_make_character(obj->scale);
  sign = obj->sign == 0 ? 
    scheme_intern_symbol("+") : scheme_intern_symbol("-");
  val = scheme_make_sized_string((char *)(obj->val),SQL_MAX_NUMERIC_LEN,TRUE);

  retval = scheme_null;
  retval = scheme_make_pair(val,retval);
  retval = scheme_make_pair(sign,retval);
  retval = scheme_make_pair(scale,retval);
  retval = scheme_make_pair(precision,retval);

  return retval;
}

Scheme_Object *readNumericBuffer(SQL_NUMERIC_STRUCT *buffer,long numElts) {
  SQL_NUMERIC_STRUCT *p;
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(makeNumericList(p),retval);
  }

  return retval;
}

Scheme_Object *makeDateList(DATE_STRUCT *obj) {
  Scheme_Object *year;
  Scheme_Object *month;
  Scheme_Object *day;
  Scheme_Object *retval;

  year = scheme_make_integer(obj->year);
  month = scheme_make_integer(obj->month);
  day = scheme_make_integer(obj->day);

  retval = scheme_null;
  retval = scheme_make_pair(day,retval);
  retval = scheme_make_pair(month,retval);
  retval = scheme_make_pair(year,retval);

  return retval;
}

Scheme_Object *readDateBuffer(DATE_STRUCT *buffer,long numElts) {
  DATE_STRUCT *p;
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(makeDateList(p),retval);
  }

  return retval;
}

Scheme_Object *makeTimeStampList(TIMESTAMP_STRUCT *obj) {
  Scheme_Object *year;
  Scheme_Object *month;
  Scheme_Object *day;
  Scheme_Object *hour;
  Scheme_Object *minute;
  Scheme_Object *second;
  Scheme_Object *fraction;
  Scheme_Object *retval;

  year = scheme_make_integer(obj->year);
  month = scheme_make_integer(obj->month);
  day = scheme_make_integer(obj->day);
  hour = scheme_make_integer(obj->hour);
  minute = scheme_make_integer(obj->minute);
  second = scheme_make_integer(obj->second);
  fraction = scheme_make_integer(obj->fraction);

  retval = scheme_null;
  retval = scheme_make_pair(fraction,retval);
  retval = scheme_make_pair(second,retval);
  retval = scheme_make_pair(minute,retval);
  retval = scheme_make_pair(hour,retval);
  retval = scheme_make_pair(day,retval);
  retval = scheme_make_pair(month,retval);
  retval = scheme_make_pair(year,retval);

  return retval;
}

Scheme_Object *readTimeStampBuffer(TIMESTAMP_STRUCT *buffer,long numElts) {
  TIMESTAMP_STRUCT *p;
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(makeTimeStampList(p),retval);
  }

  return retval;
}


Scheme_Object *makeTimeList(TIME_STRUCT *obj) {
  Scheme_Object *hour;
  Scheme_Object *minute;
  Scheme_Object *second;
  Scheme_Object *retval;

  hour = scheme_make_integer(obj->hour);
  minute = scheme_make_integer(obj->minute);
  second = scheme_make_integer(obj->second);

  retval = scheme_null;
  retval = scheme_make_pair(second,retval);
  retval = scheme_make_pair(minute,retval);
  retval = scheme_make_pair(hour,retval);

  return retval;
}

Scheme_Object *readTimeBuffer(TIME_STRUCT *buffer,long numElts) {
  TIME_STRUCT *p;
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(makeTimeList(p),retval);
  }

  return retval;
}

Scheme_Object *signToSchemeObject(SQLSMALLINT sign) {
  return sign == SQL_TRUE ? 
    scheme_intern_symbol("-") :
    scheme_intern_symbol("+");
}

char *intervalString(int val) {
  switch (val) {
  case SQL_IS_YEAR :
    return "year";
  case SQL_IS_MONTH :
    return "month";
  case SQL_IS_DAY :
    return "day";
  case SQL_IS_HOUR :
    return "hour";
  case SQL_IS_MINUTE :
    return "minute";
  case SQL_IS_SECOND :
    return "second";
  case SQL_IS_YEAR_TO_MONTH :
    return "year-to-month";
  case SQL_IS_DAY_TO_HOUR :
    return "day-to-hour";
  case SQL_IS_DAY_TO_MINUTE :
    return "day-to-minute";
  case SQL_IS_DAY_TO_SECOND :  
    return "day-to-second";
  case SQL_IS_HOUR_TO_MINUTE :
    return "hour-to-minute";
  case SQL_IS_HOUR_TO_SECOND :
    return "hour-to-second";
  case SQL_IS_MINUTE_TO_SECOND :
    return "minute-to-second";
  }

  return "unknown";
}

Scheme_Object *readInterval(SQL_INTERVAL_STRUCT *buffer,long numElts,
			    Scheme_Object *(*f)(SQL_INTERVAL_STRUCT *)) {
  SQL_INTERVAL_STRUCT *p;
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair((*f)(p),retval);
  }

  return retval;
}

void badInterval(int expected,int actual) {
  scheme_signal_error("Expect %s interval, but got %s interval",
		      intervalString(expected),
		      intervalString(actual));
}

Scheme_Object *makeIntervalYear(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *year;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_YEAR);

  sign = signToSchemeObject(obj->interval_sign);
  year = scheme_make_integer(obj->intval.year_month.year);

  retval = scheme_null;
  retval = scheme_make_pair(year,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalYearBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalYear);
}

Scheme_Object *makeIntervalMonth(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *month;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_MONTH);

  sign = signToSchemeObject(obj->interval_sign);
  month = scheme_make_integer(obj->intval.year_month.month);

  retval = scheme_null;
  retval = scheme_make_pair(month,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalMonthBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalMonth);
}


Scheme_Object *makeIntervalDay(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *day;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_DAY);

  sign = signToSchemeObject(obj->interval_sign);
  day = scheme_make_integer(obj->intval.day_second.day);

  retval = scheme_null;
  retval = scheme_make_pair(day,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalDayBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalDay);
}

Scheme_Object *makeIntervalHour(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *hour;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_HOUR);

  sign = signToSchemeObject(obj->interval_sign);
  hour = scheme_make_integer(obj->intval.day_second.hour);

  retval = scheme_null;
  retval = scheme_make_pair(hour,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalHourBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalHour);
}


Scheme_Object *makeIntervalMinute(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *minute;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_MINUTE);

  sign = signToSchemeObject(obj->interval_sign);
  minute = scheme_make_integer(obj->intval.day_second.minute);

  retval = scheme_null;
  retval = scheme_make_pair(minute,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalMinute);
}

Scheme_Object *makeIntervalSecond(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *second;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_SECOND);

  sign = signToSchemeObject(obj->interval_sign);
  second = scheme_make_integer(obj->intval.day_second.second);

  retval = scheme_null;
  retval = scheme_make_pair(second,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}


Scheme_Object *readIntervalSecondBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalSecond);
}

Scheme_Object *makeIntervalYearMonth(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *year;
  Scheme_Object *month;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_YEAR_TO_MONTH);

  sign = signToSchemeObject(obj->interval_sign);
  year = scheme_make_integer(obj->intval.year_month.year);
  month = scheme_make_integer(obj->intval.year_month.month);

  retval = scheme_null;
  retval = scheme_make_pair(month,retval);
  retval = scheme_make_pair(year,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalYearMonthBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalYearMonth);
}


Scheme_Object *makeIntervalDayHour(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *day;
  Scheme_Object *hour;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_DAY_TO_HOUR);

  sign = signToSchemeObject(obj->interval_sign);
  day = scheme_make_integer(obj->intval.day_second.day);
  hour = scheme_make_integer(obj->intval.day_second.hour);

  retval = scheme_null;
  retval = scheme_make_pair(hour,retval);
  retval = scheme_make_pair(day,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalDayHourBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalDayHour);
}

Scheme_Object *makeIntervalDayMinute(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *day;
  Scheme_Object *hour;
  Scheme_Object *minute;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_DAY_TO_MINUTE);

  sign = signToSchemeObject(obj->interval_sign);
  day = scheme_make_integer(obj->intval.day_second.day);
  hour = scheme_make_integer(obj->intval.day_second.hour);
  minute = scheme_make_integer(obj->intval.day_second.minute);

  retval = scheme_null;
  retval = scheme_make_pair(minute,retval);
  retval = scheme_make_pair(hour,retval);
  retval = scheme_make_pair(day,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalDayMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalDayMinute);
}

Scheme_Object *makeIntervalDaySecond(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *day;
  Scheme_Object *hour;
  Scheme_Object *minute;
  Scheme_Object *second;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_DAY_TO_SECOND);

  sign = signToSchemeObject(obj->interval_sign);
  day = scheme_make_integer(obj->intval.day_second.day);
  hour = scheme_make_integer(obj->intval.day_second.hour);
  minute = scheme_make_integer(obj->intval.day_second.minute);
  second = scheme_make_integer(obj->intval.day_second.second);

  retval = scheme_null;
  retval = scheme_make_pair(second,retval);
  retval = scheme_make_pair(minute,retval);
  retval = scheme_make_pair(hour,retval);
  retval = scheme_make_pair(day,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readDaySecondBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalDaySecond);
}


Scheme_Object *makeIntervalHourMinute(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *hour;
  Scheme_Object *minute;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_HOUR_TO_MINUTE);

  sign = signToSchemeObject(obj->interval_sign);
  hour = scheme_make_integer(obj->intval.day_second.hour);
  minute = scheme_make_integer(obj->intval.day_second.minute);

  retval = scheme_null;
  retval = scheme_make_pair(minute,retval);
  retval = scheme_make_pair(hour,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalHourMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalHourMinute);
}

Scheme_Object *makeIntervalHourSecond(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *hour;
  Scheme_Object *minute;
  Scheme_Object *second;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_HOUR_TO_SECOND);

  sign = signToSchemeObject(obj->interval_sign);
  hour = scheme_make_integer(obj->intval.day_second.hour);
  minute = scheme_make_integer(obj->intval.day_second.minute);
  second = scheme_make_integer(obj->intval.day_second.second);

  retval = scheme_null;
  retval = scheme_make_pair(second,retval);
  retval = scheme_make_pair(minute,retval);
  retval = scheme_make_pair(hour,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readHourSecondBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalHourSecond);
}

Scheme_Object *makeIntervalMinuteSecond(SQL_INTERVAL_STRUCT *obj) {
  Scheme_Object *sign;
  Scheme_Object *minute;
  Scheme_Object *second;
  Scheme_Object *retval;

  checkInterval(obj,SQL_IS_MINUTE_TO_SECOND);

  sign = signToSchemeObject(obj->interval_sign);
  minute = scheme_make_integer(obj->intval.day_second.minute);
  second = scheme_make_integer(obj->intval.day_second.second);

  retval = scheme_null;
  retval = scheme_make_pair(second,retval);
  retval = scheme_make_pair(minute,retval);
  retval = scheme_make_pair(sign,retval);

  return retval;
}

Scheme_Object *readIntervalMinuteSecondBuffer(SQL_INTERVAL_STRUCT *buffer,long numElts) {
  return readInterval(buffer,numElts,makeIntervalMinuteSecond);
}

Scheme_Object *readBinaryBuffer(char *buffer,long numElts) {
  char *p,*q,*retstring;
  long i;
  int lo,hi;
  static char hexCharTable[] = 
  { '0','1','2','3','4','5','6','7',
    '8','9','A','B','C','D','E','F' };

  // use string of hex digits to represent binary data

  retstring = (char *)scheme_malloc(2 * numElts + 1);

  p = buffer;
  q = retstring;

  for (i = 0; i < numElts; i++) {
    hi = *p & 0xF0;
    lo = *p++ & 0x0F;
    *q++ = hexCharTable[hi];
    *q++ = hexCharTable[lo];
  }

  *q = '\0';

  return scheme_make_string(retstring);
}

Scheme_Object *readBitBuffer(unsigned char *buffer,long numElts) {
  Scheme_Object *retval;
  unsigned char *p;
  long i;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair((*p == 0 ? scheme_false : scheme_true),retval);
  }

  return retval;
}

Scheme_Object *readSBigIntBuffer(_int64 *buffer,long numElts) {

  scheme_signal_error("Reading SQL_C_SBIGINT buffers not supported");

  return scheme_void;
}

Scheme_Object *readUBigIntBuffer(unsigned _int64 *buffer,long numElts) {

  scheme_signal_error("Reading SQL_C_UBIGINT buffers not supported");

  return scheme_void;
}

Scheme_Object *readTinyIntBuffer(char *buffer,long numElts) {
  Scheme_Object *retval;
  char *p;
  long i;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(scheme_make_integer((int)*p),retval);
  }

  return retval;
}

Scheme_Object *readUTinyIntBuffer(unsigned char *buffer,long numElts) {
  Scheme_Object *retval;
  unsigned char *p;
  long i;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    retval = scheme_make_pair(scheme_make_integer((int)*p),retval);
  }

  return retval;
}

Scheme_Object *readGUIDBuffer(SQLGUID *buffer,long numElts) {
  Scheme_Object *retval;
  SRP_SQL_GUID *guid;
  SQLGUID *p;
  long i;

  retval = scheme_null;

  for (i = 0, p = buffer; i < numElts; i++,p++) {
    guid = (SRP_SQL_GUID *)scheme_malloc(sizeof(SRP_SQL_GUID));
    guid->type = sql_guid_type;
    guid->guid = *p;
    retval = scheme_make_pair((Scheme_Object *)guid,retval);
  }

  return retval;
}
