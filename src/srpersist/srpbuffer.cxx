// srpbuffer.cxx

#ifdef WIN32
#include <windows.h>
#endif

#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

#include "escheme.h"

#include "srptypes.h"
#include "srpbuffer.h"
#include "srpersist.h"

typedef SQLUINTEGER (*INTERVAL_ACCESSOR)(SQL_INTERVAL_STRUCT *); 

Scheme_Object *readCharBuffer(char *buffer,long numElts) {
  return scheme_make_sized_string(buffer,numElts,TRUE);
}

void writeCharBuffer(char *buffer,Scheme_Object *obj) {
  strcpy((char *)buffer,SCHEME_STR_VAL(obj));
}

Scheme_Object *readLongBuffer(long *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_integer_value(buffer[i]),retval);
  }

  return retval;
}

void writeLongBuffer(long *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  long longVal;
  long i;

  currList = obj;

  for (i = 0;currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);

    if (scheme_get_int_val(currVal,&longVal) == 0) {
      scheme_signal_error("sql-write-buffer: number too big");
    } 

    buffer[i] = longVal;

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readULongBuffer(unsigned long *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_integer_value_from_unsigned(buffer[i]),retval);
  }

  return retval;
}

void writeULongBuffer(unsigned long *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  unsigned long ulongVal;
  long i;

  currList = obj;

  for (i = 0;currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);

    if (scheme_get_unsigned_int_val(currVal,&ulongVal) == 0) {
      scheme_signal_error("sql-write-buffer: number too big");
    } 

    buffer[i] = ulongVal;

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readShortBuffer(short *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_integer_value((long)(buffer[i])),retval);
  }

  return retval;
}

void writeShortBuffer(short *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  long i;

  currList = obj;

  for (i = 0; currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);

    if (isSmallInt(currVal) == FALSE) {
      scheme_signal_error("sql-write-buffer: number too big");
    } 

    buffer[i] = (short)SCHEME_INT_VAL(currVal);

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readUShortBuffer(unsigned short *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_integer_value_from_unsigned((unsigned long)(buffer[i])),retval);
  }

  return retval;
}

void writeUShortBuffer(unsigned short *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  long i;

  currList = obj;

  for (i = 0; currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);

    if (isUnsignedSmallInt(currVal) == FALSE) {
      scheme_signal_error("sql-write-buffer: number too big");
    } 

    buffer[i] = (unsigned short)SCHEME_INT_VAL(currVal);

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readFloatBuffer(float *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_double((double)(buffer[i])),retval);
  }

  return retval;
}

void writeFloatBuffer(float *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  long i;

  currList = obj;

  for (i = 0; currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);

    buffer[i] = (float)SCHEME_FLOAT_VAL(currVal);

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readDoubleBuffer(double *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_double(buffer[i]),retval);
  }

  return retval;
}

void writeDoubleBuffer(double *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  long i;

  currList = obj;

  for (i = 0; currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);

    buffer[i] = SCHEME_DBL_VAL(currVal);

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readNumericBuffer(SQL_NUMERIC_STRUCT *buffer,long numElts) {
  Scheme_Object *retval,*numStruct,*digits;
  Scheme_Object *argv[4];
  SQL_NUMERIC_STRUCT *currVal;
  long i;
  int j,k;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    currVal = buffer + i;
    argv[0] = scheme_make_integer(currVal->precision);
    argv[1] = scheme_make_integer(currVal->scale);
    argv[2] = scheme_make_integer_value_from_unsigned(currVal->sign);

    // in Scheme structure, store hex digits with MSBytes leftmost
    // in MS structure, MSBytes are rightmost

    digits = scheme_null;

    // rightmost 0's in MS structure can be stripped off

    k = sizeray(currVal->val) - 1;
    while (k >= 0) {
      if (currVal->val[k] != 0) {
	break;
      }
      k--;
    }

    for (j = 0; j <= k; j++) {
      digits = scheme_make_pair(scheme_make_integer(currVal->val[j]),digits);
    }
    argv[3] = scheme_list_to_vector(digits);  

    numStruct = scheme_make_struct_instance(NUMERIC_STRUCT_TYPE,sizeray(argv),argv);
    retval = scheme_make_pair(numStruct,retval);
  }

  return retval;
}

void writeNumericBuffer(SQL_NUMERIC_STRUCT *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  Scheme_Object *precision,*scale,*sign,*val;
  SQL_NUMERIC_STRUCT *currBuff;
  char *signStr;
  long i;
  int j,k;

  currList = obj;

  for (i = 0; currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);
    currBuff = buffer + i;

    precision = scheme_apply(NUMERIC_PRECISION,1,&currVal);
    scale = scheme_apply(NUMERIC_SCALE,1,&currVal);  
    sign = scheme_apply(NUMERIC_SIGN,1,&currVal);    
    val = scheme_apply(NUMERIC_VAL,1,&currVal);    

    if (isUnsignedCharInt(precision) == FALSE) {
      scheme_signal_error("Precision in numeric structure not exact integer or too large");
    }

    if (isCharInt(scale) == FALSE) {
      scheme_signal_error("Scale in numeric structure not exact integer or too large");
    }

    if (SCHEME_SYMBOLP(sign) == FALSE) {
      scheme_signal_error("Sign in numeric structure neither \'+ nor \'-");
    }

    signStr = SCHEME_SYM_VAL(sign);

    if (strcmp(signStr,"+") && strcmp(signStr,"-")) {
      scheme_signal_error("Sign in numeric structure neither \'+ nor \'-");
    }

    if (SCHEME_VECTORP(val) == FALSE) {
      scheme_signal_error("Value in numeric structure not a vector of exact integers");
    }

    if (SCHEME_VEC_SIZE(val) > SQL_MAX_NUMERIC_LEN) {
      scheme_signal_error("Length of value vector in numeric structure too long");
    }

    for (j = 0; j < SQL_MAX_NUMERIC_LEN; j++) {
      if (isUnsignedCharInt(SCHEME_VEC_ELS(val)[j]) == FALSE) {
	scheme_signal_error("Value in numeric structure not a vector of exact integers");
      }
    }

    currBuff->precision = (SQLCHAR)SCHEME_INT_VAL(precision);
    currBuff->scale = (SQLSCHAR)SCHEME_INT_VAL(scale);
    currBuff->sign = (*signStr == '+') ?  1 : 0;

    j = SQL_MAX_NUMERIC_LEN - 1;
    while (j >= 0) {
      if (SCHEME_INT_VAL(SCHEME_VEC_ELS(val)[j]) != 0) {
	break;
      }
      j--;
    }

    for (k = 0; j >= 0; j--,k++) {
      currBuff->val[k] = (SQLCHAR)SCHEME_INT_VAL(SCHEME_VEC_ELS(val)[j]);
    }

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readDateBuffer(SQL_DATE_STRUCT *buffer,long numElts) {
  Scheme_Object *retval,*dateStruct;
  Scheme_Object *argv[3];
  SQL_DATE_STRUCT *currVal;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    currVal = buffer + i;
    argv[0] = scheme_make_integer(currVal->year);
    argv[1] = scheme_make_integer_value_from_unsigned(currVal->month);
    argv[2] = scheme_make_integer_value_from_unsigned(currVal->day);
    dateStruct = scheme_make_struct_instance(DATE_STRUCT_TYPE,sizeray(argv),argv);
    retval = scheme_make_pair(dateStruct,retval);
  }

  return retval;
}

void writeDateBuffer(SQL_DATE_STRUCT *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  Scheme_Object *year,*month,*day;
  SQL_DATE_STRUCT *currBuff;
  long i;

  currList = obj;

  for (i = 0; currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);
    currBuff = buffer + i;

    year = scheme_apply(DATE_YEAR,1,&currVal);
    month = scheme_apply(DATE_MONTH,1,&currVal);  
    day = scheme_apply(DATE_DAY,1,&currVal);    

    if (isSmallInt(year) == FALSE) {
      scheme_signal_error("Year in date structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(month) == FALSE) {
      scheme_signal_error("Month in date structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(day) == FALSE) {
      scheme_signal_error("Day in date structure not exact integer or too large");
    }

    currBuff->year = (SQLSMALLINT)SCHEME_INT_VAL(year);
    currBuff->month = (SQLUSMALLINT)SCHEME_INT_VAL(month);
    currBuff->day = (SQLUSMALLINT)SCHEME_INT_VAL(day);

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readTimeBuffer(SQL_TIME_STRUCT *buffer,long numElts) {
  Scheme_Object *retval,*timeStruct;
  Scheme_Object *argv[3];
  SQL_TIME_STRUCT *currVal;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    currVal = buffer + i;
    argv[0] = scheme_make_integer_value_from_unsigned(currVal->hour);
    argv[1] = scheme_make_integer_value_from_unsigned(currVal->minute);
    argv[2] = scheme_make_integer_value_from_unsigned(currVal->second);
    timeStruct = scheme_make_struct_instance(TIME_STRUCT_TYPE,sizeray(argv),argv);
    retval = scheme_make_pair(timeStruct,retval);
  }

  return retval;
}

void writeTimeBuffer(SQL_TIME_STRUCT *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  Scheme_Object *hour,*minute,*second;
  SQL_TIME_STRUCT *currBuff;
  long i;

  currList = obj;

  for (i = 0; currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);
    currBuff = buffer + i;

    hour = scheme_apply(TIME_HOUR,1,&currVal);
    minute = scheme_apply(TIME_MINUTE,1,&currVal);  
    second = scheme_apply(TIME_SECOND,1,&currVal);    

    if (isUnsignedSmallInt(hour) == FALSE) {
      scheme_signal_error("Hour in time structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(minute) == FALSE) {
      scheme_signal_error("Minute in time structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(second) == FALSE) {
      scheme_signal_error("Second in time structure not exact integer or too large");
    }

    currBuff->hour = (SQLUSMALLINT)SCHEME_INT_VAL(hour);
    currBuff->minute = (SQLUSMALLINT)SCHEME_INT_VAL(minute);
    currBuff->second = (SQLUSMALLINT)SCHEME_INT_VAL(second);

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readTimeStampBuffer(SQL_TIMESTAMP_STRUCT *buffer,long numElts) {
  Scheme_Object *retval,*timeStampStruct;
  Scheme_Object *argv[7];
  SQL_TIMESTAMP_STRUCT *currVal;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    currVal = buffer + i;
    argv[0] = scheme_make_integer(currVal->year);
    argv[1] = scheme_make_integer_value_from_unsigned(currVal->month);
    argv[2] = scheme_make_integer_value_from_unsigned(currVal->day);
    argv[3] = scheme_make_integer_value_from_unsigned(currVal->hour);
    argv[4] = scheme_make_integer_value_from_unsigned(currVal->minute);
    argv[5] = scheme_make_integer_value_from_unsigned(currVal->second);
    argv[6] = scheme_make_integer_value_from_unsigned(currVal->fraction);
    timeStampStruct = scheme_make_struct_instance(TIMESTAMP_STRUCT_TYPE,sizeray(argv),argv);
    retval = scheme_make_pair(timeStampStruct,retval);
  }

  return retval;
}

void writeTimeStampBuffer(SQL_TIMESTAMP_STRUCT *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  Scheme_Object *year,*month,*day,*hour,*minute,*second,*fraction;
  SQL_TIMESTAMP_STRUCT *currBuff;
  SQLUINTEGER fractionVal;
  long i;

  currList = obj;

  for (i = 0; currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);
    currBuff = buffer + i;

    year = scheme_apply(TIMESTAMP_YEAR,1,&currVal);
    month = scheme_apply(TIMESTAMP_MONTH,1,&currVal);  
    day = scheme_apply(TIMESTAMP_DAY,1,&currVal);    
    hour = scheme_apply(TIMESTAMP_HOUR,1,&currVal);
    minute = scheme_apply(TIMESTAMP_MINUTE,1,&currVal);  
    second = scheme_apply(TIMESTAMP_SECOND,1,&currVal);    
    fraction = scheme_apply(TIMESTAMP_FRACTION,1,&currVal);    

    if (isSmallInt(year) == FALSE) {
      scheme_signal_error("Year in timestamp structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(month) == FALSE) {
      scheme_signal_error("Month in timestamp structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(day) == FALSE) {
      scheme_signal_error("Day in timestamp structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(hour) == FALSE) {
      scheme_signal_error("Hour in timestamp structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(minute) == FALSE) {
      scheme_signal_error("Minute in timestamp structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(second) == FALSE) {
      scheme_signal_error("Second in timestamp structure not exact integer or too large");
    }

    if (SCHEME_EXACT_INTEGERP(fraction) == FALSE) {
      scheme_signal_error("Fraction in timestamp structure not exact integer");

    }

    if (scheme_get_unsigned_int_val(fraction,&fractionVal) == 0) {
      scheme_signal_error("Fraction in timestamp structure too large");
    }

    currBuff->year = (SQLSMALLINT)SCHEME_INT_VAL(year);
    currBuff->month = (SQLUSMALLINT)SCHEME_INT_VAL(month);
    currBuff->day = (SQLUSMALLINT)SCHEME_INT_VAL(day);
    currBuff->hour = (SQLUSMALLINT)SCHEME_INT_VAL(hour);
    currBuff->minute = (SQLUSMALLINT)SCHEME_INT_VAL(minute);
    currBuff->second = (SQLUSMALLINT)SCHEME_INT_VAL(second);
    currBuff->fraction = fractionVal;

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readGuidBuffer(SQLGUID *buffer,long numElts) {
  Scheme_Object *retval,*guidStruct;
  Scheme_Object *argv[4];
  SQLGUID *currVal;
  long i;
  short j;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    currVal = buffer + i;
    argv[0] = scheme_make_integer_value_from_unsigned(currVal->Data1);
    argv[1] = scheme_make_integer_value_from_unsigned(currVal->Data2);
    argv[2] = scheme_make_integer_value_from_unsigned(currVal->Data3);
    argv[3] = scheme_make_vector(8,scheme_void);
    for (j = 0; j < 8; j++) {
      SCHEME_VEC_ELS(argv[3])[j] = 
	scheme_make_integer_value_from_unsigned(currVal->Data4[j]);
    }
    guidStruct = scheme_make_struct_instance(GUID_STRUCT_TYPE,sizeray(argv),argv);
    retval = scheme_make_pair(guidStruct,retval);
  }

  return retval;
}

void writeGuidBuffer(SQLGUID *buffer,Scheme_Object *obj) {
  Scheme_Object *currList,*currVal;
  Scheme_Object *Data1,*Data2,*Data3,*Data4;
  unsigned long Data1Val;
  SQLGUID *currBuff;
  long i;
  short j;

  currList = obj;

  for (i = 0; currList != scheme_null; i++) {

    currVal = SCHEME_CAR(currList);
    currBuff = buffer + i;

    Data1 = scheme_apply(GUID_DATA1,1,&currVal);
    Data2 = scheme_apply(GUID_DATA2,1,&currVal);
    Data3 = scheme_apply(GUID_DATA3,1,&currVal);
    Data4 = scheme_apply(GUID_DATA4,1,&currVal);
    
    if (SCHEME_EXACT_INTEGERP(Data1) == FALSE ||
	scheme_get_unsigned_int_val(Data1,&Data1Val) == 0) {
      scheme_signal_error("Data2 in GUID structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(Data2) == FALSE) {
      scheme_signal_error("Data2 in GUID structure not exact integer or too large");
    }

    if (isUnsignedSmallInt(Data3) == FALSE) {
      scheme_signal_error("Data3 in GUID structure not exact integer or too large");
    }

    if (SCHEME_VECTORP(Data4) == FALSE) {
      scheme_signal_error("Data4 in GUID structure not a vector of exact integers");
    }

    for (j = 0; j < 8; j++) {
      if (isUnsignedCharInt(SCHEME_VEC_ELS(Data4)[j]) == FALSE) {
	scheme_signal_error("vector element in Data4 in GUID structure not exact integer or too large");
      }
    }

    currBuff->Data1 = Data1Val;
    currBuff->Data2 = (SQLUSMALLINT)SCHEME_INT_VAL(Data2);
    currBuff->Data3 = (SQLUSMALLINT)SCHEME_INT_VAL(Data3);

    for (j = 0; j < 8; j++) {
      currBuff->Data4[j] = 
	(BYTE)SCHEME_INT_VAL(SCHEME_VEC_ELS(Data4)[j]);
    }

    currList = SCHEME_CDR(currList);
  }
}

Scheme_Object *readIntervalBuffer(SQL_INTERVAL_STRUCT *buffer,
				  long numElts,
				  Scheme_Object *structType,
				  INTERVAL_ACCESSOR *fs,
				  size_t numAcc) {
  Scheme_Object *retval,*theStruct;
  Scheme_Object *argv[3];
  SQL_INTERVAL_STRUCT *currVal;
  long i;
  size_t j;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    currVal = buffer + i;
    argv[0] = scheme_make_integer(currVal->interval_sign);
    for (j = 0; j < numAcc; j++) {
      argv[j+1] = scheme_make_integer_value_from_unsigned(fs[j](currVal));
    }
    theStruct = scheme_make_struct_instance(structType,numAcc+1,argv);
    retval = scheme_make_pair(theStruct,retval);
  }

  return retval;
}

SQLUINTEGER getIntervalYear(SQL_INTERVAL_STRUCT *p) {
  return p->intval.year_month.year;
}

Scheme_Object *readIntervalYearBuffer(SQL_INTERVAL_STRUCT *buffer,
				      long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalYear };
  return readIntervalBuffer(buffer,numElts,YEAR_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalMonth(SQL_INTERVAL_STRUCT *p) {
  return p->intval.year_month.month;
}

Scheme_Object *readIntervalMonthBuffer(SQL_INTERVAL_STRUCT *buffer,
				       long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalMonth };
  return readIntervalBuffer(buffer,numElts,MONTH_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalDay(SQL_INTERVAL_STRUCT *p) {
  return p->intval.day_second.day;
}

Scheme_Object *readIntervalDayBuffer(SQL_INTERVAL_STRUCT *buffer,
				       long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalDay };
  return readIntervalBuffer(buffer,numElts,DAY_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalHour(SQL_INTERVAL_STRUCT *p) {
  return p->intval.day_second.hour;
}

Scheme_Object *readIntervalHourBuffer(SQL_INTERVAL_STRUCT *buffer,
				      long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalHour };
  return readIntervalBuffer(buffer,numElts,HOUR_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalMinute(SQL_INTERVAL_STRUCT *p) {
  return p->intval.day_second.minute;
}

Scheme_Object *readIntervalMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,
					long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalMinute };
  return readIntervalBuffer(buffer,numElts,MINUTE_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalSecond(SQL_INTERVAL_STRUCT *p) {
  return p->intval.day_second.second;
}

Scheme_Object *readIntervalSecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalSecond };
  return readIntervalBuffer(buffer,numElts,SECOND_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalYearMonthBuffer(SQL_INTERVAL_STRUCT *buffer,
					   long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalYear,getIntervalMonth };

  return readIntervalBuffer(buffer,numElts,
			    YEAR_TO_MONTH_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalDayHourBuffer(SQL_INTERVAL_STRUCT *buffer,
					 long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalDay,getIntervalHour }; 

  return readIntervalBuffer(buffer,numElts,
			    DAY_TO_HOUR_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalDayMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,
					   long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalDay,getIntervalHour, getIntervalMinute }; 

  return readIntervalBuffer(buffer,numElts,
			    DAY_TO_MINUTE_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalDaySecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					   long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalDay,getIntervalHour, 
			      getIntervalMinute,getIntervalSecond }; 

  return readIntervalBuffer(buffer,numElts,
			    DAY_TO_SECOND_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalHourMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,
					    long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalHour,getIntervalMinute };

  return readIntervalBuffer(buffer,numElts,
			    HOUR_TO_MINUTE_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalHourSecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					    long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalHour,getIntervalMinute, getIntervalSecond };

  return readIntervalBuffer(buffer,numElts,
			    HOUR_TO_SECOND_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalMinuteSecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					      long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalMinute,getIntervalSecond };
  return readIntervalBuffer(buffer,numElts,
			    MINUTE_TO_SECOND_INTERVAL_STRUCT_TYPE,
			    acc,sizeray(acc));
}

Scheme_Object *readBinaryBuffer(char *buffer,long numElts) {
  Scheme_Object *retval;
  char *s;
  int i,j;

  // convert each byte to hex char pairs

  retval = scheme_alloc_string(numElts * 2 + 1,'\0');
  s = SCHEME_STR_VAL(retval);

  for (i = 0,j = 0; i < numElts; i++,j+=2) {
    sprintf(s + j,"%02X",(int)buffer[i]);
  }

  return retval;
}

Scheme_Object *readBitBuffer(unsigned char *buffer,long numElts) {
  Scheme_Object *retval;
  char *s;
  int i;

  retval = scheme_alloc_string(numElts + 1,'\0');
  s = SCHEME_STR_VAL(retval);

  for (i = 0; i < numElts; i++) {
    sprintf(s + i,buffer[i] ? "1" : "0");
  }

  return retval;
}

Scheme_Object *readSBigIntBuffer(_int64 *buffer,long numElts) {
  Scheme_Object *retval,*bigLo,*bigHi;
  char bigBuff[25];
  long i;
  int lo,hi;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    lo = (int)(buffer[i] & 0xFFFFFFFF);
    hi = (int)((buffer[i] >> 32) & 0xFFFFFFFF);
    bigLo = scheme_make_bignum(lo);
    bigHi = scheme_make_bignum(hi);
    sprintf(bigBuff,"%s%s",
	    scheme_bignum_to_string(bigHi,16),
	    scheme_bignum_to_string(bigLo,16));
    retval = scheme_make_pair(scheme_read_bignum(bigBuff,16),retval);
  }

  return retval;
}

Scheme_Object *readUBigIntBuffer(unsigned _int64 *buffer,long numElts) {
  Scheme_Object *retval,*bigLo,*bigHi;
  char bigBuff[25];
  long i;
  unsigned lo,hi;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    lo = (unsigned)(buffer[i] & 0xFFFFFFFF);
    hi = (unsigned)((buffer[i] >> 32) & 0xFFFFFFFF);
    bigLo = scheme_make_bignum_from_unsigned(lo);
    bigHi = scheme_make_bignum_from_unsigned(hi);
    sprintf(bigBuff,"%s%s",
	    scheme_bignum_to_string(bigHi,16),
	    scheme_bignum_to_string(bigLo,16));
    retval = scheme_make_pair(scheme_read_bignum(bigBuff,16),retval);
  }

  return retval;
}

Scheme_Object *readTinyIntBuffer(char *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_integer(buffer[i]),retval);
  }

  return retval;
}

Scheme_Object *readUTinyIntBuffer(unsigned char *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_integer_value_from_unsigned(buffer[i]),retval);
  }

  return retval;
}

