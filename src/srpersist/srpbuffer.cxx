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

Scheme_Object *readLongBuffer(long *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_integer_value(buffer[i]),retval);
  }

  return retval;
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

Scheme_Object *readShortBuffer(short *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_integer_value((long)(buffer[i])),retval);
  }

  return retval;
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

Scheme_Object *readFloatBuffer(float *buffer,long numElts) {
  Scheme_Object *retval;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    retval = scheme_make_pair(scheme_make_double((double)(buffer[i])),retval);
  }

  return retval;
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

Scheme_Object *readNumericBuffer(SQL_NUMERIC_STRUCT *buffer,long numElts) {
  Scheme_Object *retval,*numStruct,*digits;
  Scheme_Object *argv[4];
  SQL_NUMERIC_STRUCT *currVal;
  long i;
  int j;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    currVal = buffer + i;
    argv[0] = scheme_make_integer(currVal->precision);
    argv[1] = scheme_make_integer(currVal->scale);
    argv[2] = scheme_make_integer_value_from_unsigned(currVal->sign);
    digits = scheme_null;
    for (j = sizeray(currVal->val) - 1; j >= 0; j--) {
      digits = scheme_make_pair(scheme_make_integer(currVal->val[j]),digits);
    }
    argv[3] = scheme_list_to_vector(digits);  

    numStruct = scheme_make_struct_instance(numericStructType,sizeray(argv),argv);
    retval = scheme_make_pair(numStruct,retval);
  }

  return retval;
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
    dateStruct = scheme_make_struct_instance(dateStructType,sizeray(argv),argv);
    retval = scheme_make_pair(dateStruct,retval);
  }

  return retval;
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
    timeStruct = scheme_make_struct_instance(timeStructType,sizeray(argv),argv);
    retval = scheme_make_pair(timeStruct,retval);
  }

  return retval;
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
    timeStampStruct = scheme_make_struct_instance(timeStampStructType,sizeray(argv),argv);
    retval = scheme_make_pair(timeStampStruct,retval);
  }

  return retval;
}

Scheme_Object *readGUIDBuffer(SQLGUID *buffer,long numElts) {
  Scheme_Object *retval,*guidStruct;
  Scheme_Object *argv[4];
  SQLGUID *currVal;
  long i;

  retval = scheme_null;

  for (i = numElts - 1; i >= 0; i--) {
    currVal = buffer + i;
    argv[0] = scheme_make_integer_value_from_unsigned(currVal->Data1);
    argv[1] = scheme_make_integer_value_from_unsigned(currVal->Data2);
    argv[2] = scheme_make_integer_value_from_unsigned(currVal->Data3);
    argv[3] = scheme_make_sized_string((char *)currVal->Data4,8,TRUE);
    guidStruct = scheme_make_struct_instance(guidStructType,sizeray(argv),argv);
    retval = scheme_make_pair(guidStruct,retval);
  }

  return retval;
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
  return readIntervalBuffer(buffer,numElts,yearIntervalStructType,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalMonth(SQL_INTERVAL_STRUCT *p) {
  return p->intval.year_month.month;
}

Scheme_Object *readIntervalMonthBuffer(SQL_INTERVAL_STRUCT *buffer,
				       long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalMonth };
  return readIntervalBuffer(buffer,numElts,monthIntervalStructType,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalDay(SQL_INTERVAL_STRUCT *p) {
  return p->intval.day_second.day;
}

Scheme_Object *readIntervalDayBuffer(SQL_INTERVAL_STRUCT *buffer,
				       long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalDay };
  return readIntervalBuffer(buffer,numElts,dayIntervalStructType,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalHour(SQL_INTERVAL_STRUCT *p) {
  return p->intval.day_second.hour;
}

Scheme_Object *readIntervalHourBuffer(SQL_INTERVAL_STRUCT *buffer,
				      long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalHour };
  return readIntervalBuffer(buffer,numElts,hourIntervalStructType,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalMinute(SQL_INTERVAL_STRUCT *p) {
  return p->intval.day_second.minute;
}

Scheme_Object *readIntervalMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,
					long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalMinute };
  return readIntervalBuffer(buffer,numElts,minuteIntervalStructType,
			    acc,sizeray(acc));
}

SQLUINTEGER getIntervalSecond(SQL_INTERVAL_STRUCT *p) {
  return p->intval.day_second.second;
}

Scheme_Object *readIntervalSecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalSecond };
  return readIntervalBuffer(buffer,numElts,secondIntervalStructType,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalYearMonthBuffer(SQL_INTERVAL_STRUCT *buffer,
					   long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalYear,getIntervalMonth };

  return readIntervalBuffer(buffer,numElts,
			    yearToMonthIntervalStructType,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalDayHourBuffer(SQL_INTERVAL_STRUCT *buffer,
					 long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalDay,getIntervalHour }; 

  return readIntervalBuffer(buffer,numElts,
			    dayToHourIntervalStructType,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalDayMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,
					   long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalDay,getIntervalHour, getIntervalMinute }; 

  return readIntervalBuffer(buffer,numElts,
			    dayToMinuteIntervalStructType,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalDaySecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					   long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalDay,getIntervalHour, 
			      getIntervalMinute,getIntervalSecond }; 

  return readIntervalBuffer(buffer,numElts,
			    dayToSecondIntervalStructType,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalHourMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,
					    long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalHour,getIntervalMinute };

  return readIntervalBuffer(buffer,numElts,
			    hourToMinuteIntervalStructType,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalHourSecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					    long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalHour,getIntervalMinute, getIntervalSecond };

  return readIntervalBuffer(buffer,numElts,
			    hourToSecondIntervalStructType,
			    acc,sizeray(acc));
}

Scheme_Object *readIntervalMinuteSecondBuffer(SQL_INTERVAL_STRUCT *buffer,
					      long numElts) {
  INTERVAL_ACCESSOR acc[] = { getIntervalMinute,getIntervalSecond };
  return readIntervalBuffer(buffer,numElts,
			    minuteToSecondIntervalStructType,
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

