// srpersist.h

#define sizeray(x) (sizeof(x)/sizeof(*x))

#define SRP_PRIM_DECL(f) Scheme_Object *f(int,Scheme_Object **)

#define namedConstSearch(s,array) \
     (SRP_NAMED_CONSTANT *) \
     bsearch(s,array,sizeray(array),sizeof(*array), \
	     (int (*)(const void *,const void *))keyConstCmp)

#define namedSmallConstSearch(s,array) \
     (SRP_NAMED_SMALL_CONSTANT *) \
     bsearch(s,array,sizeray(array),sizeof(*array), \
	     (int (*)(const void *,const void *))keySmallConstCmp)

#define namedTypedConstSearch(s,array) \
     (SRP_NAMED_TYPED_CONSTANT *) \
     bsearch(s,array,sizeray(array),sizeof(*array), \
	     (int (*)(const void *,const void *))keyTypedConstCmp)

#define namedBitsDictSearch(s,array,asize) \
     (SRP_NAMED_BITS_DICT *) \
     bsearch(s,array,asize,sizeof(*array), \
	     (int (*)(const void *,const void *))keyBitsDictCmp)

#define namedConstSort(array) \
     qsort(array,sizeray(array),sizeof(array[0]), \
	   (int (*)(const void *,const void *))namedConstCmp)

#define namedTypedConstSort(array) \
     qsort(array,sizeray(array),sizeof(array[0]), \
	   (int (*)(const void *,const void *))namedTypedConstCmp)

#define namedBitsDictSort(array) \
     qsort(array,sizeray(array),sizeof(array[0]), \
	   (int (*)(const void *,const void *))namedBitsDictCmp)

#define NO_BIT_NAME "sql-unknown-number"

typedef struct _buffer_tbl_entry_ {
  void *address;
  SRP_SQL_BUFFER *buffer;
  struct _buffer_tbl_entry_ *next;
} SRP_BUFFER_TBL_ENTRY;

#define BUFFER_TBL_SIZE 97

typedef struct _srp_prim_ {
  Scheme_Object *(*c_fun)(int argc,Scheme_Object **);
  char *name;
  short minargs;
  short maxargs;
} SRP_PRIM;

typedef struct _srp_struct_ {
  char *name;
  Scheme_Object ***pStructFuns;
  char **fields;
  int numFields;
} SRPSTRUCT;

typedef struct _named_constant_ {
  char *scheme_name;
  SQLUINTEGER val;
} SRP_NAMED_CONSTANT;

typedef struct _named_small_constant_ {
  char *scheme_name;
  SQLSMALLINT val;
} SRP_NAMED_SMALL_CONSTANT;

typedef  enum _const_type_ { 
  sqlinteger,
  sqlusmallint,
  sqluinteger,
  sqlbool,
  namedusmallint,
  namedinteger,
  nameduinteger,
  possiblynameduinteger,
  boolstring,
  string,
  bitmask,
  henv,
  hdbc,
  hstmt,
  hdesc,
  opparms,
  rowstatus,
  sqlboxeduint,
} SRP_CONST_TYPE;

typedef struct _named_typed_constant_ {
  char *scheme_name;
  SQLUINTEGER val;
  SRP_CONST_TYPE type;
} SRP_NAMED_TYPED_CONSTANT;

typedef struct named_bits_dict_ {
  char *scheme_name;
  SRP_NAMED_CONSTANT *bits;
  size_t numBits;
} SRP_NAMED_BITS_DICT;

// Scheme_Object *scheme_make_list(Scheme_Object *,...);

// type-and-value predicates

BOOL isSmallInt(Scheme_Object *);
BOOL isUnsignedSmallInt(Scheme_Object *);
BOOL isUnsignedInt(Scheme_Object *);
BOOL isCharInt(Scheme_Object *);
BOOL isUnsignedCharInt(Scheme_Object *);

// buffer procedures

Scheme_Object *readCharBuffer(char *,long);
void writeCharBuffer(char *,Scheme_Object *);
Scheme_Object *readLongBuffer(long *,long);
void writeLongBuffer(long *,Scheme_Object *);
Scheme_Object *readULongBuffer(unsigned long *buffer,long);
void writeULongBuffer(unsigned long *,Scheme_Object *);
Scheme_Object *readShortBuffer(short *,long);
void writeShortBuffer(short *,Scheme_Object *);
Scheme_Object *readUShortBuffer(unsigned short *buffer,long);
void writeUShortBuffer(unsigned short *,Scheme_Object *);
Scheme_Object *readFloatBuffer(float *,long);
void writeFloatBuffer(float *,Scheme_Object *);
Scheme_Object *readDoubleBuffer(double *,long);
void writeDoubleBuffer(double *,Scheme_Object *);
Scheme_Object *readNumericBuffer(SQL_NUMERIC_STRUCT *,long);
void writeNumericBuffer(SQL_NUMERIC_STRUCT *,Scheme_Object *);
Scheme_Object *readDateBuffer(DATE_STRUCT *buffer,long);
void writeDateBuffer(SQL_DATE_STRUCT *,Scheme_Object *);
Scheme_Object *readTimeStampBuffer(TIMESTAMP_STRUCT *buffer,long);
void writeTimeStampBuffer(SQL_TIMESTAMP_STRUCT *,Scheme_Object *);
Scheme_Object *readTimeBuffer(TIME_STRUCT *buffer,long);
void writeTimeBuffer(SQL_TIME_STRUCT *,Scheme_Object *);
Scheme_Object *readGuidBuffer(SQLGUID *buffer,long);
void writeGuidBuffer(SQLGUID *,Scheme_Object *);
Scheme_Object *readIntervalYearBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalMonthBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalDayBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalHourBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalSecondBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalYearMonthBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalDayHourBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalDayMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalDaySecondBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalHourMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalHourSecondBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalMinuteSecondBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readBinaryBuffer(char *buffer,long);
void writeBinaryBuffer(char *buffer,Scheme_Object *);
Scheme_Object *readBitBuffer(unsigned char *buffer,long);
void writeBitBuffer(char *buffer,Scheme_Object *);
Scheme_Object *readBigIntBuffer(_int64 *buffer,long);
void writeBigIntBuffer(_int64 *buffer,Scheme_Object *);
Scheme_Object *readUBigIntBuffer(unsigned _int64 *buffer,long);
void writeUBigIntBuffer(unsigned _int64 *buffer,Scheme_Object *);
Scheme_Object *readTinyBuffer(char *buffer,long);
void writeTinyBuffer(char *buffer,Scheme_Object *);
Scheme_Object *readUTinyBuffer(unsigned char *buffer,long);
void writeUTinyBuffer(unsigned char *buffer,Scheme_Object *);

// utilities

SRP_PRIM_DECL(srp_make_indicator);
SRP_PRIM_DECL(srp_read_indicator);
SRP_PRIM_DECL(srp_set_indicator);
SRP_PRIM_DECL(srp_read_row_status);
SRP_PRIM_DECL(srp_read_boxed_uint);
SRP_PRIM_DECL(srp_read_op_parms);
SRP_PRIM_DECL(srp_make_buffer);
SRP_PRIM_DECL(srp_read_buffer);
SRP_PRIM_DECL(srp_write_buffer);

// from SQL.H

SRP_PRIM_DECL(srp_SQLAllocConnect);
SRP_PRIM_DECL(srp_SQLAllocEnv);
SRP_PRIM_DECL(srp_SQLAllocHandle);
SRP_PRIM_DECL(srp_SQLAllocStmt);
SRP_PRIM_DECL(srp_SQLBindCol);
SRP_PRIM_DECL(srp_SQLBindParam);
SRP_PRIM_DECL(srp_SQLCancel);
SRP_PRIM_DECL(srp_SQLCloseCursor);
SRP_PRIM_DECL(srp_SQLColAttribute);
SRP_PRIM_DECL(srp_SQLColumns);
SRP_PRIM_DECL(srp_SQLConnect);
SRP_PRIM_DECL(srp_SQLCopyDesc);
SRP_PRIM_DECL(srp_SQLDataSources);
SRP_PRIM_DECL(srp_SQLDescribeCol);
SRP_PRIM_DECL(srp_SQLDisconnect);
SRP_PRIM_DECL(srp_SQLEndTran);
SRP_PRIM_DECL(srp_SQLError);
SRP_PRIM_DECL(srp_SQLExecDirect);
SRP_PRIM_DECL(srp_SQLExecute);
SRP_PRIM_DECL(srp_SQLFetch);
SRP_PRIM_DECL(srp_SQLFetchScroll);
SRP_PRIM_DECL(srp_SQLFreeConnect);
SRP_PRIM_DECL(srp_SQLFreeEnv);
SRP_PRIM_DECL(srp_SQLFreeHandle);
SRP_PRIM_DECL(srp_SQLFreeStmt);
SRP_PRIM_DECL(srp_SQLGetConnectAttr);
SRP_PRIM_DECL(srp_SQLGetConnectOption);
SRP_PRIM_DECL(srp_SQLGetCursorName);
SRP_PRIM_DECL(srp_SQLGetData);
SRP_PRIM_DECL(srp_SQLGetDescField);
SRP_PRIM_DECL(srp_SQLGetDescRec);
SRP_PRIM_DECL(srp_SQLGetDiagField);
SRP_PRIM_DECL(srp_SQLGetDiagRec);
SRP_PRIM_DECL(srp_SQLGetEnvAttr);
SRP_PRIM_DECL(srp_SQLGetFunctions);
SRP_PRIM_DECL(srp_SQLGetInfo);
SRP_PRIM_DECL(srp_SQLGetStmtAttr);
SRP_PRIM_DECL(srp_SQLGetStmtOption);
SRP_PRIM_DECL(srp_SQLGetTypeInfo);
SRP_PRIM_DECL(srp_SQLNumResultCols);
SRP_PRIM_DECL(srp_SQLParamData);
SRP_PRIM_DECL(srp_SQLPrepare);
SRP_PRIM_DECL(srp_SQLPutData);
SRP_PRIM_DECL(srp_SQLRowCount);
SRP_PRIM_DECL(srp_SQLSetConnectAttr);
SRP_PRIM_DECL(srp_SQLSetConnectOption);
SRP_PRIM_DECL(srp_SQLSetCursorName);
SRP_PRIM_DECL(srp_SQLSetDescField);
SRP_PRIM_DECL(srp_SQLSetDescRec);
SRP_PRIM_DECL(srp_SQLSetEnvAttr);
SRP_PRIM_DECL(srp_SQLSetParam);
SRP_PRIM_DECL(srp_SQLSetStmtAttr);
SRP_PRIM_DECL(srp_SQLSetStmtOption);
SRP_PRIM_DECL(srp_SQLSpecialColumns);
SRP_PRIM_DECL(srp_SQLStatistics);
SRP_PRIM_DECL(srp_SQLTables);
SRP_PRIM_DECL(srp_SQLTransact);

     // from SQLEXT.H)
	
SRP_PRIM_DECL(srp_SQLDriverConnect);
SRP_PRIM_DECL(srp_SQLBrowseConnect);
SRP_PRIM_DECL(srp_SQLColAttributes);
SRP_PRIM_DECL(srp_SQLColumnPrivileges);
SRP_PRIM_DECL(srp_SQLDescribeParam);
SRP_PRIM_DECL(srp_SQLExtendedFetch);
SRP_PRIM_DECL(srp_SQLForeignKeys);
SRP_PRIM_DECL(srp_SQLMoreResults);
SRP_PRIM_DECL(srp_SQLNativeSql);
SRP_PRIM_DECL(srp_SQLNumParams);
SRP_PRIM_DECL(srp_SQLParamOptions);
SRP_PRIM_DECL(srp_SQLPrimaryKeys);
SRP_PRIM_DECL(srp_SQLProcedureColumns);
SRP_PRIM_DECL(srp_SQLProcedures);
SRP_PRIM_DECL(srp_SQLSetPos);
SRP_PRIM_DECL(srp_SQLTablePrivileges);
SRP_PRIM_DECL(srp_SQLDrivers);
SRP_PRIM_DECL(srp_SQLBindParameter);
SRP_PRIM_DECL(srp_SQLSetScrollOptions);
SRP_PRIM_DECL(srp_SQLLenBinaryAttr);

// structure info

extern Scheme_Object **numericStructFuns;
extern Scheme_Object **dateStructFuns;
extern Scheme_Object **timeStructFuns;
extern Scheme_Object **timeStampStructFuns;
extern Scheme_Object **guidStructFuns;
extern Scheme_Object **yearIntervalStructFuns;
extern Scheme_Object **monthIntervalStructFuns;
extern Scheme_Object **dayIntervalStructFuns;
extern Scheme_Object **hourIntervalStructFuns;
extern Scheme_Object **minuteIntervalStructFuns;
extern Scheme_Object **secondIntervalStructFuns;
extern Scheme_Object **yearToMonthIntervalStructFuns;
extern Scheme_Object **dayToHourIntervalStructFuns;
extern Scheme_Object **dayToMinuteIntervalStructFuns;
extern Scheme_Object **dayToSecondIntervalStructFuns;
extern Scheme_Object **hourToMinuteIntervalStructFuns;
extern Scheme_Object **hourToSecondIntervalStructFuns;
extern Scheme_Object **minuteToSecondIntervalStructFuns;

// structure #defines

#define TYPE 0
#define PI_1 3
#define PI_2 5
#define PI_3 7
#define PI_4 9
#define PI_5 11
#define PI_6 13
#define PI_7 15
#define PI_8 17
#define PI_9 19


#define NUMERIC_STRUCT_TYPE numericStructFuns[TYPE]
#define NUMERIC_PRECISION   numericStructFuns[PI_1]
#define NUMERIC_SCALE       numericStructFuns[PI_2]
#define NUMERIC_SIGN        numericStructFuns[PI_3]
#define NUMERIC_VAL         numericStructFuns[PI_4]

#define DATE_STRUCT_TYPE    dateStructFuns[TYPE]
#define DATE_YEAR           dateStructFuns[PI_1]
#define DATE_MONTH          dateStructFuns[PI_2]
#define DATE_DAY            dateStructFuns[PI_3]

#define TIME_STRUCT_TYPE    timeStructFuns[TYPE]
#define TIME_HOUR           timeStructFuns[PI_1]
#define TIME_MINUTE         timeStructFuns[PI_2]
#define TIME_SECOND         timeStructFuns[PI_3]

#define TIMESTAMP_STRUCT_TYPE  timeStampStructFuns[TYPE]
#define TIMESTAMP_YEAR         timeStampStructFuns[PI_1]
#define TIMESTAMP_MONTH        timeStampStructFuns[PI_2]
#define TIMESTAMP_DAY          timeStampStructFuns[PI_3]
#define TIMESTAMP_HOUR         timeStampStructFuns[PI_4]
#define TIMESTAMP_MINUTE       timeStampStructFuns[PI_5]
#define TIMESTAMP_SECOND       timeStampStructFuns[PI_6]
#define TIMESTAMP_FRACTION     timeStampStructFuns[PI_7]

#define GUID_STRUCT_TYPE    guidStructFuns[TYPE]
#define GUID_DATA1          guidStructFuns[PI_1]
#define GUID_DATA2          guidStructFuns[PI_2]
#define GUID_DATA3          guidStructFuns[PI_3]
#define GUID_DATA4          guidStructFuns[PI_4]

#define YEAR_INTERVAL_STRUCT_TYPE  yearIntervalStructFuns[TYPE]
#define YEAR_INTERVAL_SIGN         yearIntervalStructFuns[PI_1]
#define YEAR_INTERVAL_YEAR         yearIntervalStructFuns[PI_2]

#define MONTH_INTERVAL_STRUCT_TYPE  monthIntervalStructFuns[TYPE]
#define MONTH_INTERVAL_SIGN         monthIntervalStructFuns[PI_1]
#define MONTH_INTERVAL_MONTH        monthIntervalStructFuns[PI_2]

#define DAY_INTERVAL_STRUCT_TYPE    dayIntervalStructFuns[TYPE]
#define DAY_INTERVAL_SIGN           dayIntervalStructFuns[PI_1]
#define DAY_INTERVAL_DAY            dayIntervalStructFuns[PI_2]

#define HOUR_INTERVAL_STRUCT_TYPE   hourIntervalStructFuns[TYPE]
#define HOUR_INTERVAL_SIGN          hourIntervalStructFuns[PI_1]
#define HOUR_INTERVAL_HOUR          hourIntervalStructFuns[PI_2]

#define MINUTE_INTERVAL_STRUCT_TYPE  minuteIntervalStructFuns[TYPE]
#define MINUTE_INTERVAL_SIGN         minuteIntervalStructFuns[PI_1]
#define MINUTE_INTERVAL_MINUTE       minuteIntervalStructFuns[PI_2]

#define SECOND_INTERVAL_STRUCT_TYPE  secondIntervalStructFuns[TYPE]
#define SECOND_INTERVAL_SIGN         secondIntervalStructFuns[PI_1]
#define SECOND_INTERVAL_SECOND       secondIntervalStructFuns[PI_2]

#define YEAR_TO_MONTH_INTERVAL_STRUCT_TYPE  yearToMonthIntervalStructFuns[TYPE]
#define YEAR_TO_MONTH_INTERVAL_SIGN         yearToMonthIntervalStructFuns[PI_1]
#define YEAR_TO_MONTH_INTERVAL_YEAR         yearToMonthIntervalStructFuns[PI_2]
#define YEAR_TO_MONTH_INTERVAL_MONTH        yearToMonthIntervalStructFuns[PI_3]

#define DAY_TO_HOUR_INTERVAL_STRUCT_TYPE  dayToHourIntervalStructFuns[TYPE]
#define DAY_TO_HOUR_INTERVAL_SIGN         dayToHourIntervalStructFuns[PI_1]
#define DAY_TO_HOUR_INTERVAL_DAY          dayToHourIntervalStructFuns[PI_2]
#define DAY_TO_HOUR_INTERVAL_HOUR         dayToHourIntervalStructFuns[PI_3]

#define DAY_TO_MINUTE_INTERVAL_STRUCT_TYPE  dayToMinuteIntervalStructFuns[TYPE]
#define DAY_TO_MINUTE_INTERVAL_SIGN         dayToMinuteIntervalStructFuns[PI_1]
#define DAY_TO_MINUTE_INTERVAL_DAY          dayToMinuteIntervalStructFuns[PI_2]
#define DAY_TO_MINUTE_INTERVAL_HOUR         dayToMinuteIntervalStructFuns[PI_3]
#define DAY_TO_MINUTE_INTERVAL_MINUTE       dayToMinuteIntervalStructFuns[PI_4]

#define DAY_TO_SECOND_INTERVAL_STRUCT_TYPE  dayToSecondIntervalStructFuns[TYPE]
#define DAY_TO_SECOND_INTERVAL_SIGN         dayToSecondIntervalStructFuns[PI_1]
#define DAY_TO_SECOND_INTERVAL_DAY          dayToSecondIntervalStructFuns[PI_2]
#define DAY_TO_SECOND_INTERVAL_HOUR         dayToSecondIntervalStructFuns[PI_3]
#define DAY_TO_SECOND_INTERVAL_MINUTE       dayToSecondIntervalStructFuns[PI_4]
#define DAY_TO_SECOND_INTERVAL_SECOND       dayToSecondIntervalStructFuns[PI_5]

#define HOUR_TO_MINUTE_INTERVAL_STRUCT_TYPE  hourToMinuteIntervalStructFuns[TYPE]
#define HOUR_TO_MINUTE_INTERVAL_SIGN         hourToMinuteIntervalStructFuns[PI_1]
#define HOUR_TO_MINUTE_INTERVAL_HOUR         hourToMinuteIntervalStructFuns[PI_2]
#define HOUR_TO_MINUTE_INTERVAL_MINUTE       hourToMinuteIntervalStructFuns[PI_3]

#define HOUR_TO_SECOND_INTERVAL_STRUCT_TYPE  hourToSecondIntervalStructFuns[TYPE]
#define HOUR_TO_SECOND_INTERVAL_SIGN         hourToSecondIntervalStructFuns[PI_1]
#define HOUR_TO_SECOND_INTERVAL_HOUR         hourToSecondIntervalStructFuns[PI_2]
#define HOUR_TO_SECOND_INTERVAL_MINUTE       hourToSecondIntervalStructFuns[PI_3]
#define HOUR_TO_SECOND_INTERVAL_SECOND       hourToSecondIntervalStructFuns[PI_4]

#define MINUTE_TO_SECOND_INTERVAL_STRUCT_TYPE  minuteToSecondIntervalStructFuns[TYPE]
#define MINUTE_TO_SECOND_INTERVAL_SIGN         minuteToSecondIntervalStructFuns[PI_1]
#define MINUTE_TO_SECOND_INTERVAL_MINUTE       minuteToSecondIntervalStructFuns[PI_2]
#define MINUTE_TO_SECOND_INTERVAL_SECOND       minuteToSecondIntervalStructFuns[PI_3]
     










