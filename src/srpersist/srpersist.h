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
  apdesc,
  ardesc,
  ipdesc,
  irdesc,
  operationarray,
  paramsprocessed,
  rowstatus,
  rowsfetched,
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

// buffer procedures

Scheme_Object *readCharBuffer(char *,long);
Scheme_Object *readLongBuffer(long *,long);
Scheme_Object *readShortBuffer(short *,long);
Scheme_Object *readFloatBuffer(float *,long);
Scheme_Object *readDoubleBuffer(double *,long);
Scheme_Object *readNumericBuffer(SQL_NUMERIC_STRUCT *,long);
Scheme_Object *readDateBuffer(DATE_STRUCT *buffer,long);
Scheme_Object *readTimeStampBuffer(TIMESTAMP_STRUCT *buffer,long);
Scheme_Object *readTimeBuffer(TIME_STRUCT *buffer,long);
Scheme_Object *readIntervalYearBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalMonthBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalDayBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalHourBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalSecondBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalYearMonthBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalDayHourBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalDayMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readDaySecondBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalHourMinuteBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readHourSecondBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readIntervalMinuteSecondBuffer(SQL_INTERVAL_STRUCT *buffer,long);
Scheme_Object *readBinaryBuffer(char *buffer,long);
Scheme_Object *readBitBuffer(unsigned char *buffer,long);
Scheme_Object *readSBigIntBuffer(_int64 *buffer,long);
Scheme_Object *readUBigIntBuffer(unsigned _int64 *buffer,long);
Scheme_Object *readTinyIntBuffer(char *buffer,long);
Scheme_Object *readULongBuffer(unsigned long *buffer,long);
Scheme_Object *readUShortBuffer(unsigned short *buffer,long);
Scheme_Object *readUTinyIntBuffer(unsigned char *buffer,long);
Scheme_Object *readBookmarkBuffer(unsigned long *buffer,long);
Scheme_Object *readVarBookmarkBuffer(unsigned char **buffer,long);
Scheme_Object *readGUIDBuffer(SQLGUID *buffer,long);

// utilities

SRP_PRIM_DECL(srp_make_indicator);
SRP_PRIM_DECL(srp_read_indicator);
SRP_PRIM_DECL(srp_set_indicator);
SRP_PRIM_DECL(srp_read_row_status);
SRP_PRIM_DECL(srp_read_boxed_uint);
SRP_PRIM_DECL(srp_make_buffer);
SRP_PRIM_DECL(srp_read_buffer);

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

