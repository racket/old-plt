// srpersist.cpp 

#include "stdafx.h"

#include <limits.h>
#include <stdio.h>

#ifdef WIN32
#include <windows.h>
#endif

#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

#include "escheme.h"

#include "srpersist.h"
#include "srptypes.h"

#include "srpprims.tbl"
#include "srpconsts.tbl"
#include "srpinfo.tbl"
#include "srpbitmask.tbl"

int keyConstCmp(char *s,SRP_NAMED_CONSTANT *p) {
  return stricmp(s,p->scheme_name);
}

int keySmallConstCmp(char *s,SRP_NAMED_SMALL_CONSTANT *p) {
  return stricmp(s,p->scheme_name);
} 

int keyTypedConstCmp(char *s,SRP_NAMED_TYPED_CONSTANT *p) {
  return stricmp(s,p->scheme_name);
}

int keyBitsDictCmp(char *s,SRP_NAMED_BITS_DICT *p) {
  return stricmp(s,p->scheme_name);
}

int namedBitsDictCmp(SRP_NAMED_BITS_DICT *p1,SRP_NAMED_BITS_DICT *p2) {
  return stricmp(p1->scheme_name,p2->scheme_name);
}

char *findBitInDict(SQLUINTEGER value,SRP_NAMED_CONSTANT *entry,size_t numBits) {
  size_t i;

  for (i = 0; i < numBits; i++,entry++) {
    if (entry->val == value) {
      return entry->scheme_name;
    }
  } 

  return "<none>";
}

char *findSmallIntName(char *name,SQLUSMALLINT value,
		       SRP_NAMED_BITS_DICT *dict,size_t dictsize) {
  SRP_NAMED_BITS_DICT *p;

  p = namedBitsDictSearch(name,dict,dictsize);

  if (p == NULL) {
    scheme_signal_error("Unknown constant dictionary entry: %s",name);
  }

  return findBitInDict(value,p->bits,p->numBits);
}

char *findIntegerName(char *name,SQLUINTEGER value,
		      SRP_NAMED_BITS_DICT *dict,size_t dictsize) {
  SRP_NAMED_BITS_DICT *p;

  p = namedBitsDictSearch(name,dict,dictsize);

  if (p == NULL) {
    scheme_signal_error("Unknown constant dictionary entry: %s",name);
  }

  return findBitInDict(value,p->bits,p->numBits);
}

Scheme_Object *bitsListFromBitMask(char *name,SQLUINTEGER bits) {
  Scheme_Object *retval;
  SRP_NAMED_BITS_DICT *p;
  size_t numBits;
  SRP_NAMED_CONSTANT *q;
  size_t i;

  p = namedBitsDictSearch(name,bitMaskTable,sizeray(bitMaskTable));

  if (p == NULL) {
    scheme_signal_error("Unknown constant dictionary entry: %s",name);
  }

  numBits = p->numBits;

  retval = scheme_null;

  for (i = 0, q = p->bits; i < numBits; i++,q++) {
    if (q->val & bits) {
      retval = scheme_make_pair(scheme_intern_symbol(q->scheme_name),
				retval);
    }
  }

  return retval;
}

void SchemeObjectToHandle(Scheme_Object *obj,
			  HANDLE *handle,SQLSMALLINT *handleType) {

  if (SQL_HENVP(obj)) {
    *handle = SQL_HENV_VAL(obj);
    *handleType = SQL_HANDLE_ENV;
  }
  else if (SQL_HDBCP(obj)) {
    *handle = SQL_HDBC_VAL(obj);
    *handleType = SQL_HANDLE_DBC;
  }
  else if (SQL_HSTMTP(obj)) {
    *handle = SQL_HSTMT_VAL(obj);
    *handleType = SQL_HANDLE_STMT;
  }
  else if (SQL_HDESCP(obj)) {
    *handle = SQL_HDESC_VAL(obj);
    *handleType = SQL_HANDLE_DESC;
  }
}

int sizeofCDataType(SQLSMALLINT type) {
  switch (type) {
  case SQL_C_CHAR :
    return sizeof(unsigned char);
  case SQL_C_SSHORT :
    return sizeof(short int);
  case SQL_C_USHORT :
    return sizeof(unsigned short int);
  case SQL_C_SLONG :
    return sizeof(long int);
  case SQL_C_ULONG :
    // SQL_C_BOOKMARK is same value
    return sizeof(unsigned long int);
  case SQL_C_FLOAT :
    return sizeof(float);
  case SQL_C_DOUBLE :
    return sizeof(double);
  case SQL_C_BIT :
    return sizeof(unsigned char);
  case SQL_C_STINYINT :
    return sizeof(signed char);
  case SQL_C_UTINYINT :
    return sizeof(unsigned char);
  case SQL_C_SBIGINT :
    return sizeof(_int64);
  case SQL_C_UBIGINT :
    return sizeof(unsigned _int64);
  case SQL_C_BINARY :
    // SQL_C_VARBOOKMARK has same value
    return sizeof(unsigned char *);
  case SQL_C_TYPE_DATE :
    return sizeof(DATE_STRUCT);
  case SQL_C_TYPE_TIME :
    return sizeof(TIME_STRUCT);
  case SQL_C_TYPE_TIMESTAMP :
    return sizeof(TIMESTAMP_STRUCT);
  case SQL_C_NUMERIC :
    return sizeof(SQL_NUMERIC_STRUCT);
  case SQL_C_GUID :
    return sizeof(SQLGUID);
  case SQL_C_INTERVAL_YEAR :
  case SQL_C_INTERVAL_MONTH :
  case SQL_C_INTERVAL_DAY :
  case SQL_C_INTERVAL_HOUR :
  case SQL_C_INTERVAL_MINUTE :
  case SQL_C_INTERVAL_SECOND :
  case SQL_C_INTERVAL_YEAR_TO_MONTH :
  case SQL_C_INTERVAL_DAY_TO_HOUR :
  case SQL_C_INTERVAL_DAY_TO_MINUTE :
  case SQL_C_INTERVAL_DAY_TO_SECOND :
  case SQL_C_INTERVAL_HOUR_TO_MINUTE :
  case SQL_C_INTERVAL_HOUR_TO_SECOND :
  case SQL_C_INTERVAL_MINUTE_TO_SECOND :
    return sizeof(SQL_INTERVAL_STRUCT);
  }
  
  scheme_signal_error("Unknown C data type constant: %X",(int)type);

  return 0;  // keep compiler happy
}

// utilities

Scheme_Object *srp_make_indicator(int argc,Scheme_Object **argv) {
  SRP_SQL_INDICATOR *retval;

  retval = (SRP_SQL_INDICATOR *)scheme_malloc(sizeof(SRP_SQL_INDICATOR));

  retval->type = sql_indicator_type; 
  retval->value = 0;

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_read_indicator(int argc,Scheme_Object **argv) {
  SQLINTEGER value;
  Scheme_Object *value_object;

  if (SQL_INDICATORP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-read-indicator","sql-indicator",0,argc,argv);
  }

  value = SQL_INDICATOR_VAL(argv[0]);

  switch(value) {
  case SQL_NO_TOTAL :
    return scheme_intern_symbol("sql-no-total");
  case SQL_NULL_DATA :
    return scheme_intern_symbol("sql-null-data");
  case SQL_NTS :
    return scheme_intern_symbol("sql-nts");
  case SQL_COLUMN_IGNORE :
    return scheme_intern_symbol("sql-column-ignore");
  case SQL_DATA_AT_EXEC :
    return scheme_intern_symbol("sql-data-at-exec");
  }

  value_object = scheme_make_integer_value(value);

  if (value <= SQL_LEN_DATA_AT_EXEC_OFFSET) {
    return scheme_make_pair(scheme_intern_symbol("sql-len-data-at-exec"),
			    scheme_make_pair(value_object,scheme_null));
  }

  return value_object;
}

Scheme_Object *srp_set_indicator(int argc,Scheme_Object **argv) {
  char *lenString;
  SRP_SQL_INDICATOR *indicator;

  if (SQL_INDICATORP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-indicator","sql-indicator",0,argc,argv);
  }

  if (SQL_INDICATORP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-indicator","symbol",1,argc,argv);
  }

  indicator = (SRP_SQL_INDICATOR *)(argv[0]);

  lenString = SCHEME_SYM_VAL(argv[1]);

  if (strcmpi(lenString,"sql-len-data-at-exec") == 0) {
    int execVal;

    if (argc != 3) {
      scheme_signal_error("sql-set-indicator: "
			  "%s requires additional int argument",lenString);
    }

    if (SCHEME_INTP(argv[2]) == FALSE) {
      scheme_wrong_type("sql-set-indicator","int",2,argc,argv);
    }

    execVal = SCHEME_INT_VAL(argv[2]);

    indicator->value = SQL_LEN_DATA_AT_EXEC(execVal);

    return scheme_void;
  }

  if (argc != 2) {
      scheme_signal_error("sql-set-indicator: "
			  "too many arguments for indicator value %s",
			  lenString);
  }

  if (strcmpi(lenString,"sql-nts") == 0) {
    indicator->value = SQL_NTS;
  }
  else if (strcmpi(lenString,"sql-null-data") == 0) {
    indicator->value = SQL_NULL_DATA;
  }
  else if (strcmpi(lenString,"sql-column-ignore") == 0) {
    indicator->value = SQL_COLUMN_IGNORE;
  }
  else if (strcmpi(lenString,"sql-data-at-exec") == 0) {
    indicator->value = SQL_DATA_AT_EXEC;
  }
  else {
    scheme_signal_error("sql-set-indicator: "
			"unknown indicator value %s",
			lenString);

  }

  return scheme_void;
}


Scheme_Object *srp_make_buffer(int argc,Scheme_Object **argv) {
  SRP_SQL_BUFFER *retval;
  char *typeName;
  SRP_NAMED_CONSTANT *p;
  long numElts;

  if (SCHEME_SYMBOLP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-make-buffer","symbol",0,argc,argv);
  }

  if (SCHEME_EXACT_INTEGERP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-make-buffer","int",1,argc,argv);
  }

  typeName = SCHEME_SYM_VAL(argv[0]);

  p = namedConstSearch(typeName,CDataTypes);
  
  if (p == NULL) {
    scheme_signal_error("sql-make-buffer: invalid C type: %s",typeName);
  }

  if (scheme_get_int_val(argv[1],&numElts) == 0) {
    scheme_signal_error("sql-make-buffer: too many elements requested");
  }

  retval = (SRP_SQL_BUFFER *)scheme_malloc(sizeof(SRP_SQL_BUFFER));
  retval->type = sql_buffer_type;

  retval->numElts = numElts;

  retval->CDataType = (SQLSMALLINT)(p->val);

  retval->eltSize = sizeofCDataType(retval->CDataType);

  retval->storage = scheme_malloc(retval->numElts * sizeof(retval->eltSize));

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_read_buffer(int argc,Scheme_Object **argv) {
  SQLSMALLINT CDataType;
  void *buffer;
  unsigned long numElts;

  if (SQL_BUFFERP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-read-buffer","int",0,argc,argv);
  } 

  CDataType = SQL_BUFFER_CTYPE(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[0]);
  numElts = SQL_BUFFER_NUMELTS(argv[0]);

  switch(CDataType) {
  case SQL_C_CHAR :
    return readCharBuffer((char *)buffer,numElts);
  case SQL_C_SLONG :
  case SQL_C_LONG :
    return readLongBuffer((long *)buffer,numElts);
  case SQL_C_ULONG :
    // SQL_C_BOOKMARK is the same
    return readULongBuffer((unsigned long *)buffer,numElts);
  case SQL_C_SSHORT :
  case SQL_C_SHORT :
    return readShortBuffer((short *)buffer,numElts);
  case SQL_C_USHORT :
    return readUShortBuffer((unsigned short *)buffer,numElts);
  case SQL_C_FLOAT :
    return readFloatBuffer((float *)buffer,numElts);
  case SQL_C_DOUBLE :
    return readDoubleBuffer((double *)buffer,numElts);
  case SQL_C_NUMERIC :
    return readNumericBuffer((SQL_NUMERIC_STRUCT *)buffer,numElts);
  case SQL_C_DATE :
  case SQL_C_TYPE_DATE :
    return readDateBuffer((DATE_STRUCT *)buffer,numElts);
  case SQL_C_TIME :
  case SQL_C_TYPE_TIME :
    return readTimeBuffer((TIME_STRUCT *)buffer,numElts);
  case SQL_C_TIMESTAMP :
  case SQL_C_TYPE_TIMESTAMP :
    return readTimeStampBuffer((TIMESTAMP_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_YEAR :
    return readIntervalYearBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_MONTH :
    return readIntervalMonthBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_DAY :
    return readIntervalDayBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_HOUR :
    return readIntervalHourBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_MINUTE :
    return readIntervalMinuteBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_SECOND :
    return readIntervalSecondBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_YEAR_TO_MONTH :
    return readIntervalYearMonthBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_DAY_TO_HOUR :
    return readIntervalDayHourBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_DAY_TO_MINUTE :
    return readIntervalDayMinuteBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_DAY_TO_SECOND :
    return readDaySecondBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_HOUR_TO_MINUTE :
    return readIntervalHourMinuteBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_HOUR_TO_SECOND :
    return readHourSecondBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_INTERVAL_MINUTE_TO_SECOND :
    return readIntervalMinuteSecondBuffer((SQL_INTERVAL_STRUCT *)buffer,numElts);
  case SQL_C_BINARY :
    // SQL_C_VARBOOKMARK is the same
    return readBinaryBuffer((char *)buffer,numElts);
  case SQL_C_BIT :
    return readBitBuffer((unsigned char *)buffer,numElts);
  case SQL_C_SBIGINT :
    return readSBigIntBuffer((_int64 *)buffer,numElts);
  case SQL_C_UBIGINT :
    return readUBigIntBuffer((unsigned _int64 *)buffer,numElts);
  case SQL_C_STINYINT :
  case SQL_C_TINYINT :
    return readTinyIntBuffer((char *)buffer,numElts);
  case SQL_C_UTINYINT :
    return readUTinyIntBuffer((unsigned char *)buffer,numElts);
  case SQL_C_GUID :
    return readGUIDBuffer((SQLGUID *)buffer,numElts);
  }

  scheme_signal_error("Unknown buffer C data type: %X",CDataType);

  return scheme_void; // keep compiler happy

}


// actual ODBC procedures

Scheme_Object *srp_SQLLenBinaryAttr(int argc,Scheme_Object **argv) {
  if (SCHEME_INTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-len-binary-attr","int",0,argc,argv);
  }

  // Scheme equivalent of SQL_LEN_BINARY_ATTR macro in SQLEXT.H

  return scheme_make_integer_value(-100L - SCHEME_INT_VAL(argv[0])); 
}

void scheme_add_prim_to_env(Scheme_Env *env,
			    Scheme_Object *(*f)(int,Scheme_Object **),
			    char *name,
			    short minArgs,short maxArgs) {
  Scheme_Object *pobj;

  pobj = scheme_make_prim_w_arity(f,name,minArgs,maxArgs);
    
  scheme_add_global(name,pobj,env);
}

char *nullableToString(SQLSMALLINT nullable) {
  
  switch(nullable) {
  case SQL_NO_NULLS :
    return "sql-no-nulls";
    
  case SQL_NULLABLE :
    return "sql-nullable";

  default :
    return "sql-nullable-unknown";
  }
}

char *rowStatusToString(SQLUSMALLINT rowStatus) {
  switch (rowStatus) {

  case SQL_ROW_DELETED :
    return "sql-row-deleted";

  case SQL_ROW_ERROR :
    return "sql-row-error";

  case SQL_ROW_SUCCESS :
    return "sql-row-success";

  case SQL_ROW_UPDATED :
    return "sql-row-updated";
  }

  return "?";
}

int namedConstCmp(SRP_NAMED_CONSTANT *p1,SRP_NAMED_CONSTANT *p2) {
  return stricmp(p1->scheme_name,p2->scheme_name);
}

int namedTypedConstCmp(SRP_NAMED_TYPED_CONSTANT *p1,SRP_NAMED_TYPED_CONSTANT *p2) {
  return stricmp(p1->scheme_name,p2->scheme_name);
}

BOOL isSmallInt(Scheme_Object *s) {
  long val;
  short smallVal;

  if (SCHEME_INTP(s) == FALSE) {
    return FALSE;
  }

  val = SCHEME_INT_VAL(s);

  smallVal = (short)val;

  if (smallVal != val) {
    return FALSE;
  }
    
  return TRUE;
}

BOOL isUnsignedInt(Scheme_Object *obj) {
  unsigned long val;

  if (SCHEME_EXACT_INTEGERP(obj) == FALSE ||
      scheme_get_unsigned_int_val(obj,&val) == 0) {
    return FALSE;
  }

  return TRUE;
}

BOOL isUnsignedSmallInt(Scheme_Object *obj) {
  long val;
  unsigned short smallVal;

  if (SCHEME_INTP(obj) == FALSE) {
    return FALSE;
  }

  val = SCHEME_INT_VAL(obj);

  smallVal = (unsigned short)val;

  if (smallVal != val) {
    return FALSE;
  }
    
  return TRUE;
}

void checkSQLReturn(SQLRETURN sr,char *f) {

  switch (sr) {

  case SQL_SUCCESS :
    return;

  case SQL_SUCCESS_WITH_INFO :
    return;

  case SQL_NO_DATA :
    scheme_signal_error("SQL_NO_DATA error in %s",f);

  case SQL_INVALID_HANDLE :
    scheme_signal_error("SQL_INVALID_HANDLE error in %s",f);

  case SQL_NEED_DATA :
    scheme_signal_error("SQL_NEED_DATA error in %s",f);

  case SQL_ERROR :
    scheme_signal_error("Unspecified error in %s",f);

  default :
    ;
  }

}

// Functions in SQL.H

Scheme_Object *srp_SQLAllocConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLHDBC connectHandle;
  SRP_SQL_HDBC *retval;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-alloc-connect","sql_henv",0,argc,argv);
  }

  envHandle = SQL_HENV_VAL(argv[0]);

  sr = SQLAllocConnect(envHandle,&connectHandle);

  checkSQLReturn(sr,"sql-alloc-connect");

  retval = (SRP_SQL_HDBC *)scheme_malloc(sizeof(SRP_SQL_HDBC));
  retval->type = sql_hdbc_type;
  retval->hdbc = connectHandle;

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_SQLAllocEnv(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SRP_SQL_HENV *retval;

  sr = SQLAllocEnv(&envHandle);

  checkSQLReturn(sr,"sql-alloc-env");

  retval = (SRP_SQL_HENV *)scheme_malloc(sizeof(SRP_SQL_HENV));
  retval->type = sql_henv_type;
  retval->henv = envHandle;

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_SQLAllocHandle(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  char *handleTypeString;

  if (SCHEME_SYMBOLP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-alloc-handle","symbol",0,argc,argv);
  }

  handleTypeString = SCHEME_SYM_VAL(argv[0]);

  if (stricmp(handleTypeString,"sql-handle-env") == 0) {
    SQLHENV envHandle;
    SRP_SQL_HENV *retval;
    
    if (argc > 1) {
      scheme_signal_error("In sql-alloc-handle, when first argument is "
			  "'sql-handle-env, no other argument is allowed"); 
    }

    sr = SQLAllocHandle(SQL_HANDLE_ENV,SQL_NULL_HANDLE,&envHandle);

    checkSQLReturn(sr,"sql-alloc-handle");

    retval = (SRP_SQL_HENV *)scheme_malloc(sizeof(SRP_SQL_HENV));
    retval->type = sql_henv_type;
    retval->henv = envHandle;

    return (Scheme_Object *)retval;

  }

  if (argc < 2) {
    scheme_signal_error("In sql-alloc-handle, when first argument is "
			"not 'sql-handle-env, another argument is required"); 
  }

  if (stricmp(handleTypeString,"sql-handle-dbc") == 0) {
    SQLHDBC dbcHandle;
    SRP_SQL_HDBC *retval;
    
    if (SQL_HENVP(argv[1]) == FALSE) {
      scheme_wrong_type("sql-alloc-handle","sql-henv",1,argc,argv);
    }

    sr = SQLAllocHandle(SQL_HANDLE_DBC,SQL_HENV_VAL(argv[1]),&dbcHandle);

    checkSQLReturn(sr,"sql-alloc-handle");

    retval = (SRP_SQL_HDBC *)scheme_malloc(sizeof(SRP_SQL_HDBC));
    retval->type = sql_hdbc_type;
    retval->hdbc = dbcHandle;

    return (Scheme_Object *)retval;

  }

  if (stricmp(handleTypeString,"sql-handle-stmt") == 0) {
    SQLHSTMT stmtHandle;
    SRP_SQL_HSTMT *retval;
    
    if (SQL_HDBCP(argv[1]) == FALSE) {
      scheme_wrong_type("sql-alloc-handle","sql-hdbc",1,argc,argv);
    }

    sr = SQLAllocHandle(SQL_HANDLE_STMT,SQL_HDBC_VAL(argv[1]),&stmtHandle);

    checkSQLReturn(sr,"sql-alloc-handle");

    retval = (SRP_SQL_HSTMT *)scheme_malloc(sizeof(SRP_SQL_HSTMT));
    retval->type = sql_hstmt_type;
    retval->hstmt = stmtHandle;

    return (Scheme_Object *)retval;

  }

  if (stricmp(handleTypeString,"sql-handle-desc") == 0) {
    SQLHDESC descHandle;
    SRP_SQL_HDESC *retval;
    
    if (SQL_HDBCP(argv[1]) == FALSE) {
      scheme_wrong_type("sql-alloc-handle","sql-hdbc",1,argc,argv);
    }

    sr = SQLAllocHandle(SQL_HANDLE_DESC,SQL_HDBC_VAL(argv[1]),&descHandle);

    checkSQLReturn(sr,"sql-alloc-handle");

    retval = (SRP_SQL_HDESC *)scheme_malloc(sizeof(SRP_SQL_HDESC));
    retval->type = sql_hdesc_type;
    retval->hdesc = descHandle;

    return (Scheme_Object *)retval;

  }

  scheme_signal_error("Handle type must be one of "
		      "'sql-handle-env, " 
		      "'sql-handle-dbc, "
		      "'sql-handle-stmt, or "
		      "'sql-handle-desc");

  return scheme_void; // keep compiler happy
}

Scheme_Object *srp_SQLAllocStmt(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectHandle;
  SQLHSTMT stmtHandle;
  SRP_SQL_HSTMT *retval;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-alloc-stmt","sql_hdbc",0,argc,argv);
  }

  connectHandle = SQL_HDBC_VAL(argv[0]);

  sr = SQLAllocStmt(connectHandle,&stmtHandle);

  checkSQLReturn(sr,"sql-alloc-stmt");

  retval = (SRP_SQL_HSTMT *)scheme_malloc(sizeof(SRP_SQL_HSTMT));
  retval->type = sql_hstmt_type;
  retval->hstmt = stmtHandle;

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_SQLBindCol(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  int colNumber;
  void *buffer;
  long buflen;
  int buftype;
  SQLINTEGER *indicator;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-bind-col","sql_hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-bind-col","unsigned-small-int",1,argc,argv);
  }
   
  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-bind-col","sql-buffer",2,argc,argv);
  }

  if (SQL_INDICATORP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-bind-col","sql-indicator",3,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  colNumber = SCHEME_INT_VAL(argv[1]);

  buffer = SQL_BUFFER_VAL(argv[2]);
  buflen = SQL_BUFFER_LEN(argv[2]);
  buftype = SQL_BUFFER_CTYPE(argv[2]);

  indicator = &SQL_INDICATOR_VAL(argv[3]);

  sr = SQLBindCol(stmtHandle,colNumber,buftype,buffer,buflen,indicator);

  checkSQLReturn(sr,"sql-bind-col");

  return scheme_void;
}

Scheme_Object *srp_SQLBindParam(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SRP_NAMED_SMALL_CONSTANT *p;
  char *cTypeName,*SQLTypeName;
  SQLSMALLINT cTypeVal,SQLTypeVal;
  short paramNum;
  short decimalDigits;
  unsigned long lengthPrecision;
  void *buffer;
  long retval;

  if (SQL_HSTMTP(argv[0])) {
    scheme_wrong_type("sql-bind-param","sql-hstmt",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-bind-param","small-int",1,argc,argv);
  }    

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-bind-param","symbol",2,argc,argv);
  }
   
  if (SCHEME_SYMBOLP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-bind-param","symbol",3,argc,argv);
  }
   
  if (SCHEME_INTP(argv[4]) == FALSE) {
    scheme_wrong_type("sql-bind-param","int",4,argc,argv);
  }
   
  if (isSmallInt(argv[5]) == FALSE) {
    scheme_wrong_type("sql-bind-param","int",5,argc,argv);
  }
   
  if (SQL_BUFFERP(argv[6]) == FALSE) {
    scheme_wrong_type("sql-bind-param","sql-buffer",6,argc,argv);
  }
   
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  paramNum = (short)(SCHEME_INT_VAL(argv[1]));

  cTypeName = SCHEME_SYM_VAL(argv[2]);

  p = namedSmallConstSearch(cTypeName,CDataTypes);
 
  if (p == NULL) {
    scheme_signal_error("sql-bind-col: invalid C data type name %s",cTypeName);
  }

  cTypeVal = (SQLSMALLINT)(p->val);

  SQLTypeName = SCHEME_SYM_VAL(argv[3]);

  p = namedSmallConstSearch(SQLTypeName,SQLDataTypes);
 
  if (p == NULL) {
    scheme_signal_error("sql-bind-col: invalid SQL data type name %s",SQLTypeName);
  }

  SQLTypeVal = (SQLSMALLINT)(p->val);

  scheme_get_unsigned_int_val(argv[4],&lengthPrecision);

  decimalDigits = (short)SCHEME_INT_VAL(argv[5]);

  buffer = SQL_BUFFER_VAL(argv[6]);

  sr = SQLBindParam(stmtHandle,paramNum,cTypeVal,SQLTypeVal,
		    lengthPrecision,decimalDigits,buffer,
		    &retval);

  checkSQLReturn(sr,"sql-bind-param");

  return scheme_make_integer_value(retval);

}

Scheme_Object *srp_SQLCancel(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-cancel","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLCancel(stmtHandle);

  checkSQLReturn(sr,"sql-cancel");
  
  return scheme_void;
}

Scheme_Object *srp_SQLCloseCursor(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-close-cursor","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLCloseCursor(stmtHandle);

  checkSQLReturn(sr,"sql-close-cancel");
  
  return scheme_void;
}

Scheme_Object *srp_SQLColAttribute(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT colNumber;
  SQLUSMALLINT fieldId;
  char *fieldIdString;
  char buff[2048];
  SQLSMALLINT bufflen;
  SQLINTEGER numBuffer;
  SQLSMALLINT actualLen;
  SRP_NAMED_TYPED_CONSTANT *p;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-col-attribute","sql-hstmt",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-col-attribute","small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-col-attribute","symbol",2,argc,argv);
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedTypedConstSearch(fieldIdString,colAttributes);

  if (p == NULL) {
    scheme_signal_error("Invalid column attribute: %s",fieldIdString);
  }
    
  fieldId = (SQLUSMALLINT)(p->val);
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  switch(p->type) {

  case sqlbool :

    bufflen = SQL_IS_INTEGER;
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    checkSQLReturn(sr,"sql-col-attribute");		       
    return (numBuffer == SQL_FALSE) ? scheme_false : scheme_true;

  case sqlinteger :

    bufflen = SQL_IS_INTEGER;
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    checkSQLReturn(sr,"sql-col-attribute");		       
    return scheme_make_integer_value((long)numBuffer);

  case namedinteger :

    bufflen = SQL_IS_INTEGER;
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    checkSQLReturn(sr,"sql-col-attribute");		       

    return scheme_intern_symbol(findIntegerName(fieldIdString,numBuffer,
						namedColAttrIntegers,
						sizeray(namedColAttrIntegers)));

  case string :

    bufflen = sizeof(buff);
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    checkSQLReturn(sr,"sql-col-attribute");		       
    return scheme_make_string(buff);
  }

  scheme_signal_error("sql-col-attribute: invalid attribute type");

  return scheme_void; // keep compiler happy
}

Scheme_Object *srp_SQLColumns(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalog;
  SQLSMALLINT catalogLen;
  SQLCHAR *schema;
  SQLSMALLINT schemaLen;
  SQLCHAR *table;
  SQLSMALLINT tableLen;
  SQLCHAR *column;
  SQLSMALLINT columnLen;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-columns","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 4; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-columns","string",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  catalog = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogLen = SCHEME_STRLEN_VAL(argv[1]);
  schema = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaLen = SCHEME_STRLEN_VAL(argv[2]);
  table = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableLen = SCHEME_STRLEN_VAL(argv[3]);
  column = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  columnLen = SCHEME_STRLEN_VAL(argv[4]);

  sr = SQLColumns(stmtHandle,
		  catalog,catalogLen,
		  schema,schemaLen,
		  table,tableLen,
		  column,columnLen);

  checkSQLReturn(sr,"sql-columns");		       

  return scheme_void;
}

Scheme_Object *srp_SQLConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC dbcHandle;
  SQLCHAR *server;
  SQLSMALLINT serverLen;
  SQLCHAR *user;
  SQLSMALLINT userLen;
  SQLCHAR *password;
  SQLSMALLINT passwordLen;
  int i;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-columns","sql-hdbc",0,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-columns","string",i,argc,argv);
    }
  }

  dbcHandle = SQL_HDBC_VAL(argv[0]);
  server = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  serverLen = SCHEME_STRLEN_VAL(argv[1]);
  user = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  userLen = SCHEME_STRLEN_VAL(argv[2]);
  password = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  passwordLen = SCHEME_STRLEN_VAL(argv[3]);

  sr = SQLConnect(dbcHandle,
		  server,serverLen,
		  user,userLen,
		  password,passwordLen);
		  
  checkSQLReturn(sr,"sql-connect");		       

  return scheme_void;
}

Scheme_Object *srp_SQLCopyDesc(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDESC srcDescHandle,targetDescHandle;
  int i;

  for (i = 0; i <= 1; i++) {
    if (SQL_HDESCP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-copy-desc","sql-hdesc",i,argc,argv);
    }
  }

  srcDescHandle = SQL_HDESC_VAL(argv[0]);
  targetDescHandle = SQL_HDESC_VAL(argv[1]);

  sr = SQLCopyDesc(srcDescHandle,targetDescHandle);

  checkSQLReturn(sr,"sql-copy-desc");		       

  return scheme_void;
}

Scheme_Object *srp_SQLDataSources(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLUSMALLINT direction;
  char *directionString;
  SQLCHAR server[SQL_MAX_DSN_LENGTH + 1];
  SQLCHAR description[SQL_MAX_DSN_LENGTH + 1];
  SQLSMALLINT serverLen,descriptionLen;
  SRP_NAMED_SMALL_CONSTANT *p;
  Scheme_Object *retval;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-data-sources","sql-henv",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-data-sources","symbol",0,argc,argv);
  }

  directionString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(directionString,fetchDirections);

  if (p == NULL) {
    scheme_signal_error("sql-data-sources: invalid direction: %s",
			directionString);
  }

  direction = p->val;

  envHandle = SQL_HENV_VAL(argv[0]);

  sr = SQLDataSources(envHandle,direction,
		      server,sizeof(server),&serverLen,
		      description,sizeof(description),&descriptionLen);

  checkSQLReturn(sr,"sql-data-sources");		       

  retval = scheme_make_pair(scheme_make_string((const char *)description),
			    scheme_null);
  retval = scheme_make_pair(scheme_make_string((const char *)server),
			    retval);

  return retval;
			  
}

Scheme_Object *srp_SQLDescribeCol(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT colNumber;
  SQLCHAR columnName[256];
  SQLSMALLINT colNameLen;
  SQLSMALLINT dataType;
  SQLUINTEGER colSize;
  SQLSMALLINT decimalDigits;
  SQLSMALLINT nullable;
  char *nullableString;
  char *dataTypeString;
  int i;
  Scheme_Object *retval;
  
  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-describe-col","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-describe-col","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);

  sr = SQLDescribeCol(stmtHandle,colNumber,
		      columnName,sizeof(columnName),&colNameLen,
		      &dataType,&colSize,&decimalDigits,
		      &nullable);

  checkSQLReturn(sr,"sql-describe-col");		       

  dataTypeString = "?";

  for (i = 0; i < sizeray(SQLDataTypes); i++) {
    if (dataType == SQLDataTypes[i].val) {
      dataTypeString = SQLDataTypes[i].scheme_name;
      break;
    }
  }

  nullableString = nullableToString(nullable);

  retval = scheme_null;
  retval = scheme_make_pair(scheme_intern_symbol(nullableString),retval);
  retval = scheme_make_pair(scheme_make_integer_value(decimalDigits),retval);
  retval = scheme_make_pair(scheme_make_integer_value_from_unsigned(colSize),retval);
  retval = scheme_make_pair(scheme_intern_symbol(dataTypeString),retval);
  retval = scheme_make_pair(scheme_make_string((const char *)columnName),retval);
  
  return retval;
}

Scheme_Object *srp_SQLDisconnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC dbcHandle;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-disconnect","sql-hdbc",0,argc,argv);
  }

  dbcHandle = SQL_HDBC_VAL(argv[0]);

  sr = SQLDisconnect(dbcHandle);

  checkSQLReturn(sr,"sql-disconnect");
  
  return scheme_void;
}

Scheme_Object *srp_SQLEndTran(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLSMALLINT actionType;
  char *action;

  if (SQL_HDBCP(argv[0]) == FALSE && SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-end-tran","sql-hdbc> or <sql-henv",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-end-tran","symbol",1,argc,argv);
  }

  action = SCHEME_SYM_VAL(argv[1]);

  if (stricmp(action,"sql-commit") == 0) {
    actionType = SQL_COMMIT;
  }
  else if (stricmp(action,"sql-rollback") == 0) {
    actionType = SQL_ROLLBACK;
  }
  else {
    scheme_signal_error("sql-end-tran: invalid completion type: %s",
			action);
  }

  if (SQL_HDBCP(argv[0])) {
    sr = SQLEndTran(SQL_HANDLE_DBC,SQL_HDBC_VAL(argv[0]),actionType);
  }
  else if (SQL_HENVP(argv[0])) {
    sr = SQLEndTran(SQL_HANDLE_ENV,SQL_HENV_VAL(argv[0]),actionType);
  }

  checkSQLReturn(sr,"sql-end-tran");
  
  return scheme_void;
}

Scheme_Object *srp_SQLError(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLHDBC connectionHandle;
  SQLHSTMT stmtHandle;
  SQLCHAR state[6];
  SQLINTEGER nativeError;
  SQLCHAR text[2048];
  SQLSMALLINT textLen;
  Scheme_Object *retval;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-error","sql-henv",0,argc,argv);
  }

  if (SQL_HDBCP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-error","sql-hdbc",1,argc,argv);
  }

  if (SQL_HSTMTP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-error","sql-hstmt",2,argc,argv);
  }

  envHandle = SQL_HENV_VAL(argv[0]);
  connectionHandle = SQL_HDBC_VAL(argv[1]);
  stmtHandle = SQL_HSTMT_VAL(argv[2]);

  sr = SQLError(envHandle,connectionHandle,stmtHandle,
		state,&nativeError,
		text,sizeof(text),&textLen);
		
  checkSQLReturn(sr,"sql-error");

  retval = scheme_null;
  retval = scheme_make_pair(scheme_make_string((const char *)text),
			    retval);
  retval = scheme_make_pair(scheme_make_integer_value(nativeError),
			    retval);
  retval = scheme_make_pair(scheme_make_string((const char *)state),
			    retval);

  return retval;
}

Scheme_Object *srp_SQLExecDirect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *stmt;
  SQLINTEGER stmtLen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-exec-direct","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-exec-direct","string",1,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  stmt = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  stmtLen = SCHEME_STRLEN_VAL(argv[1]);

  sr = SQLExecDirect(stmtHandle,stmt,stmtLen);

  checkSQLReturn(sr,"sql-exec-direct");  

  return scheme_void;
}

Scheme_Object *srp_SQLExecute(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-execute","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLCloseCursor(stmtHandle);

  checkSQLReturn(sr,"sql-execute");
  
  return scheme_void;
}

Scheme_Object *srp_SQLFetch(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-fetch","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLFetch(stmtHandle);

  checkSQLReturn(sr,"sql-fetch");
  
  return scheme_void;
}

Scheme_Object *srp_SQLFetchScroll(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER offset;
  char *orientationString;
  SRP_NAMED_SMALL_CONSTANT *p;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-fetch-scroll","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-fetch-scroll","symbol",1,argc,argv);
  }

  orientationString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(orientationString,fetchScrolls);

  if (p == NULL) {
    scheme_signal_error("sql-fetch-scroll: invalid orientation: %s",
			orientationString);
  } 

  if (p->val == SQL_FETCH_ABSOLUTE || p->val == SQL_FETCH_RELATIVE || 
      p->val == SQL_FETCH_BOOKMARK) {
    if (argc != 3) {
      scheme_signal_error("sql-fetch-scroll: given orientation %s "
			  "requires offset",
			  orientationString);
    }
    if (SCHEME_EXACT_INTEGERP(argv[2]) == FALSE) {
      scheme_wrong_type("sql-fetch-scroll","int",2,argc,argv);
      if (scheme_get_int_val(argv[2],&offset) == 0) {
	scheme_signal_error("sql-fetch-scroll: offset too large");
      }
    }
  }
  else {
    offset = 0;
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  offset = SCHEME_INT_VAL(argv[1]);

  sr = SQLFetchScroll(stmtHandle,p->val,offset);

  checkSQLReturn(sr,"sql-fetch-scroll");  

  return scheme_void;
}

Scheme_Object *srp_SQLFreeConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-free-connect","sql-hdbc",0,argc,argv);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  sr = SQLFreeConnect(connectionHandle);

  checkSQLReturn(sr,"sql-free-connect");  

  return scheme_void;
}

Scheme_Object *srp_SQLFreeEnv(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-free-env","sql-henv",0,argc,argv);
  }

  envHandle = SQL_HENV_VAL(argv[0]);

  sr = SQLFreeEnv(envHandle);

  checkSQLReturn(sr,"sql-free-env");  

  return scheme_void;
}

Scheme_Object *srp_SQLFreeHandle(int argc,Scheme_Object **argv) {
  SQLRETURN sr;

  if (SQL_HENVP(argv[0])) {
    sr = SQLFreeHandle(SQL_HANDLE_ENV,SQL_HENV_VAL(argv[0]));
  }
  else if (SQL_HDBCP(argv[0])) {
    sr = SQLFreeHandle(SQL_HANDLE_DBC,SQL_HDBC_VAL(argv[0]));
  }
  else if (SQL_HSTMTP(argv[0])) {
    sr = SQLFreeHandle(SQL_HANDLE_STMT,SQL_HSTMT_VAL(argv[0]));
  }
  else if (SQL_HDESCP(argv[0])) {
    sr = SQLFreeHandle(SQL_HANDLE_DESC,SQL_HDESC_VAL(argv[0]));
  }
  else {
    scheme_wrong_type("sql-free-handle",
		      "sql-henv> or <sql-hdbc> or <sql-hstmt> or <sql-hdesc",
		      0,argc,argv);
  }
    
  checkSQLReturn(sr,"sql-free-handle");  

  return scheme_void;
}

Scheme_Object *srp_SQLFreeStmt(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT hstmt;
  SQLUSMALLINT option;
  char *optionString;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-free-stmt","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-free-stmt","symbol",1,argc,argv);
  }

  optionString = SCHEME_SYM_VAL(argv[1]);

  if (stricmp(optionString,"sql-close")) {
    option = SQL_CLOSE;
  }
  else if (stricmp(optionString,"sql-drop")) {
    option = SQL_DROP;
  }
  else if (stricmp(optionString,"sql-unbind")) {
    option = SQL_UNBIND;
  }
  else if (stricmp(optionString,"sql-reset-params")) {
    option = SQL_RESET_PARAMS;
  }
  else {
    scheme_signal_error("sql-free-stmt: invalid option: %s",optionString);
  }

  hstmt = SQL_HSTMT_VAL(argv[0]);

  sr = SQLFreeStmt(hstmt,option);

  checkSQLReturn(sr,"sql-free-stmt");  

  return scheme_void;
}

Scheme_Object *srp_SQLGetConnectAttr(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  HDBC connectionHandle;
  SQLINTEGER attribute;
  char *attributeString;
  SQLPOINTER buffer;
  SQLINTEGER bufLen;
  SQLINTEGER actualLen;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-connect-attr","sql-hdbc",0,argc,argv);
  }
  
  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-connect-attr","symbol",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-connect-attr","sql-buffer",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[3]) == FALSE) {
    SRP_NAMED_CONSTANT *p;
    char *buffLenSym;

    buffLenSym = SCHEME_SYM_VAL(argv[3]);

    p = namedConstSearch(buffLenSym,buffLenTypes);

    if (p == NULL) {
      scheme_signal_error("Invalid buffer type symbol: %s",buffLenSym);
    }
    
    bufLen = p->val;
  }
  else if (isSmallInt(argv[3])) {
    bufLen = (SQLSMALLINT)SCHEME_INT_VAL(argv[3]);
  }
  else {
    scheme_wrong_type("sql-get-connect-attr","small-int or sym",3,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  if (stricmp(attributeString,"sql-attr-auto-ipd") == 0) {
    attribute = SQL_ATTR_AUTO_IPD;
  }
  else if (stricmp(attributeString,"sql-attr-connection-dead") == 0) {
    attribute = SQL_ATTR_CONNECTION_DEAD;
  }
  else {
    SRP_NAMED_CONSTANT *p;
    
    p = namedConstSearch(attributeString,settableConnectionAttributes);
  
    if (p == NULL) {
      scheme_signal_error("sql-get-connect-attr: invalid attribute: %s",
			  attributeString);
    }

    attribute = p->val;
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  buffer = SQL_BUFFER_VAL(argv[2]);

  sr = SQLGetConnectAttr(connectionHandle,attribute,buffer,bufLen,&actualLen);

  checkSQLReturn(sr,"sql-get-connect-attr");  			 
  
  return scheme_make_integer_value(actualLen);
}

Scheme_Object *srp_SQLGetConnectOption(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  HDBC connectionHandle;
  SQLUSMALLINT option;
  SQLPOINTER buffer;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-connect-option","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE && isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-connect-option","sym or unsigned-small-int",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-connect-option","sql-buffer",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *optionString;

    optionString = SCHEME_SYM_VAL(argv[1]);

    if (stricmp(optionString,"sql-attr-auto-ipd") == 0) {
      option = SQL_ATTR_AUTO_IPD;
    }
    else if (stricmp(optionString,"sql-attr-connection-dead") == 0) {
      option = SQL_ATTR_CONNECTION_DEAD;
    }
    else {
      SRP_NAMED_CONSTANT *p;
    
      p = namedConstSearch(optionString,settableConnectionAttributes);
  
      if (p == NULL) {
	scheme_signal_error("sql-get-connect-option: invalid option: %s",
			    optionString);
      }

      option = (SQLUSMALLINT)(p->val);
    }

  }
  else { /* must be unsigned small int */
    option = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[2]);

  sr = SQLGetConnectOption(connectionHandle,option,buffer);

  checkSQLReturn(sr,"sql-get-connect-option");  			 

  return scheme_void;
}

Scheme_Object *srp_SQLGetCursorName(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR name[1024];
  SQLSMALLINT actualLen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-cursor-name","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLGetCursorName(stmtHandle,name,sizeray(name),&actualLen);

  checkSQLReturn(sr,"sql-get-cursor-name");

  return scheme_make_string((const char *)name);
}

Scheme_Object *srp_SQLGetData(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT colNumber;
  SQLPOINTER buffer;
  SQLINTEGER bufferlen;
  SQLSMALLINT buffertype;
  SQLINTEGER *indicator;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-data","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-data","unsigned-small-int",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-data","sql-buffer",2,argc,argv);
  }

  if (SQL_INDICATORP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-get-data","sql-indicator",3,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferlen = SQL_BUFFER_LEN(argv[2]);
  buffertype = SQL_BUFFER_CTYPE(argv[2]);

  indicator = &SQL_INDICATOR_VAL(argv[3]);

  sr = SQLGetData(stmtHandle,colNumber,buffertype,buffer,bufferlen,indicator);

  checkSQLReturn(sr,"sql-get-data");

  return scheme_void;
}

Scheme_Object *srp_SQLGetDescField(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDESC descHandle;
  SQLSMALLINT recNumber;
  SQLSMALLINT fieldId;
  char *fieldIdString;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  SQLINTEGER actualLen;
  SRP_NAMED_SMALL_CONSTANT *p;

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-desc-field","sql-hdesc",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-desc-field","small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-desc-field","symbol",2,argc,argv);
  }

  if (SQL_BUFFERP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-get-desc-field","sql-buffer",2,argc,argv);
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedSmallConstSearch(fieldIdString,fieldDescriptors);

  if (p == NULL) {
    scheme_signal_error("sql-get-desc-field: invalid field identifier: %s",
			fieldIdString);
  }

  fieldId = p->val;

  descHandle = SQL_HDESC_VAL(argv[0]);
  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);
  buffer = SQL_BUFFER_VAL(argv[3]);
  bufferLen = SQL_BUFFER_LEN(argv[3]);

  sr = SQLGetDescField(descHandle,recNumber,fieldId,
		       buffer,bufferLen,&actualLen);

  return scheme_void;

}

Scheme_Object *srp_SQLGetDescRec(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDESC descHandle;
  SQLSMALLINT recNumber;
  SQLCHAR name[1024];
  SQLSMALLINT actualLen;
  SQLSMALLINT type;
  char *typeString;
  SQLSMALLINT subtype;
  char *subtypeString;
  SQLINTEGER length;
  SQLSMALLINT precision;
  SQLSMALLINT scale;
  SQLSMALLINT nullable; 
  char *nullableString;
  Scheme_Object *retval;
  int i;

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-desc-rec","sql-hdesc",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-desc-rec","small-int",1,argc,argv);
  }
  
  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);
  if (recNumber > SQL_DESC_COUNT) {
    scheme_signal_error("sql-get-desc-rec: invalid record number: %d",(int)recNumber);
  }

  descHandle = SQL_HDESC_VAL(argv[0]);

  sr = SQLGetDescRec(descHandle,recNumber,name,sizeray(name),&actualLen,
		     &type,&subtype,&length,&precision,
		     &scale,&nullable);

  checkSQLReturn(sr,"sql-get-desc-rec");

  typeString = "?";

  for (i = 0; i < sizeray(SQLDataTypes); i++) {
    if (SQLDataTypes[i].val == type) {
      typeString = SQLDataTypes[i].scheme_name;
      break;
    }
  }

  subtypeString = "?";

  for (i = 0; i < sizeray(datetimeIntervalCodes); i++) {
    if (datetimeIntervalCodes[i].val == subtype) {
      subtypeString = datetimeIntervalCodes[i].scheme_name;
      break;
    }
  }

  nullableString = nullableToString(nullable);

  retval = scheme_null;
  retval = scheme_make_pair(scheme_intern_symbol(nullableString),retval);
  retval = scheme_make_pair(scheme_make_integer_value(scale),retval);
  retval = scheme_make_pair(scheme_make_integer_value(precision),retval);
  retval = scheme_make_pair(scheme_make_integer_value(length),retval);
  retval = scheme_make_pair(scheme_intern_symbol(subtypeString),retval);
  retval = scheme_make_pair(scheme_intern_symbol(typeString),retval);
  retval = scheme_make_pair(scheme_make_string((const char *)name),retval);
  
  return retval;
}

Scheme_Object *srp_SQLGetDiagField(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLSMALLINT handleType;
  SQLHANDLE handle;
  SQLSMALLINT recNumber;
  SQLSMALLINT diagId;
  char *diagIdString;
  SQLPOINTER buffer;
  SQLSMALLINT bufferLen;
  SQLSMALLINT actualLen;

  if (SQL_HENVP(argv[0]) == FALSE && 
      SQL_HDBCP(argv[0]) == FALSE &&
      SQL_HSTMTP(argv[0]) == FALSE &&
      SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-diag-field",
		      "sql-henv> or <sql-hdbc> or <sql-hstmt> or <sql-hdesc",
		      0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-diag-field",
		      "small-int",1,argc,argv);
  }

  if (isSmallInt(argv[2]) == FALSE && SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-diag-field","sym or small-int",2,argc,argv);
  }

  if (SQL_BUFFERP(argv[3])) {
    scheme_wrong_type("sql-get-diag-field","sql-buffer",3,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2])) {
    SRP_NAMED_SMALL_CONSTANT *p;

    diagIdString = SCHEME_SYM_VAL(argv[2]);

    p = namedSmallConstSearch(diagIdString,diagFields);

    if (p == NULL) {
      scheme_signal_error("sql-get-diag-field: invalid diagnostic id: %s",
			  diagIdString);
    }

    diagId = p->val;
  }
  else {  
    diagId = (SQLSMALLINT)SCHEME_INT_VAL(argv[2]);
  }
    
  SchemeObjectToHandle(argv[0],&handle,&handleType);

  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  buffer = SQL_BUFFER_VAL(argv[3]);
  bufferLen = (SQLSMALLINT)SQL_BUFFER_LEN(argv[3]);

  sr = SQLGetDiagField(handleType,handle,recNumber,diagId,
		       buffer,bufferLen,&actualLen);

  checkSQLReturn(sr,"sql-get-diag-field");		       

  return scheme_void;

}

Scheme_Object *srp_SQLGetDiagRec(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLSMALLINT handleType;
  SQLHANDLE handle;
  SQLSMALLINT recNumber;
  SQLCHAR sqlState[6];
  SQLINTEGER nativeError;
  SQLCHAR messageText[1024];
  SQLSMALLINT actualLen;
  Scheme_Object *retval;

  if (SQL_HENVP(argv[0]) == FALSE && 
      SQL_HDBCP(argv[0]) == FALSE &&
      SQL_HSTMTP(argv[0]) == FALSE &&
      SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-diag-rec",
		      "sql-henv> or <sql-hdbc> or <sql-hstmt> or <sql-hdesc",
		      0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-diag-rec",
		      "small-int",1,argc,argv);
  }

  SchemeObjectToHandle(argv[0],&handle,&handleType);

  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  sqlState[5] = '\0';

  sr = SQLGetDiagRec(handleType,handle,recNumber,
		     sqlState,&nativeError,
		     messageText,sizeray(messageText),
		     &actualLen);

  checkSQLReturn(sr,"sql-get-diag-rec");		       

  retval = scheme_null;
  retval = scheme_make_pair(scheme_make_string((const char *)messageText),retval);
  retval = scheme_make_pair(scheme_make_integer(nativeError),retval);
  retval = scheme_make_pair(scheme_make_string((const char *)sqlState),retval);

  return retval;
}

Scheme_Object *srp_SQLGetEnvAttr(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLINTEGER attribute;
  SQLUINTEGER value;
  char *attributeString;
  SRP_NAMED_CONSTANT *p;
  SQLINTEGER actualLen;

  if (SQL_HENVP(argv[0]) == FALSE) { 
    scheme_wrong_type("sql-get-env-attr","sql-henv",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) { 
    scheme_wrong_type("sql-get-env-attr","symbol",1,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  p = namedConstSearch(attributeString,envAttributes);

  if (p == NULL) {
    scheme_signal_error("sql-get-env-attr: invalid environment attribute: %s",
			attributeString);
  }

  attribute = p->val;
  envHandle = SQL_HENV_VAL(argv[0]);

  // even though some attributes may be strings, all
  // current ones are int's

  sr = SQLGetEnvAttr(envHandle,attribute,&value,0,&actualLen);

  checkSQLReturn(sr,"sql-get-env-attr");		       

  return scheme_make_integer_value_from_unsigned(value);
}

Scheme_Object *srp_SQLGetFunctions(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLUSMALLINT function;
  char *functionString;
  SQLUSMALLINT supported[SQL_API_ODBC3_ALL_FUNCTIONS_SIZE];
  SRP_NAMED_SMALL_CONSTANT *p;  
  int i;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-functions","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-functions","symbol",1,argc,argv);
  }

  functionString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(functionString,sqlFunctions);
  
  if (p == NULL) {
    scheme_signal_error("sql-get-functions: invalid function name: %s",
			functionString);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  function = p->val;
  
  sr = SQLGetFunctions(connectionHandle,function,supported);

  checkSQLReturn(sr,"sql-get-functions");		       

  if (function == SQL_API_ALL_FUNCTIONS) {
    Scheme_Object *value;
    Scheme_Object *retval;
    int ndx;

    retval = scheme_null;

    for (i = 0; i < sizeray(sqlFunctions); i++) {
      ndx = sqlFunctions[i].val;
      if (ndx < 100) { // valid ODBC 2 function
	if (supported[ndx] == SQL_TRUE) {
	  value = scheme_true;
	}
	else {
	  value = scheme_false;
	}
	retval = scheme_make_pair(scheme_make_pair(scheme_intern_symbol(sqlFunctions[i].scheme_name),
						   value),
				  retval);
      }
    }

    return retval;
  }
  else if (function == SQL_API_ODBC3_ALL_FUNCTIONS) {
    Scheme_Object *value;
    Scheme_Object *retval;

    retval = scheme_null;

    for (i = 0; i < sizeray(sqlFunctions); i++) {
      if (SQL_FUNC_EXISTS(supported,sqlFunctions[i].val) == SQL_TRUE) {
	value = scheme_true;
      }
      else {
	value = scheme_false;
      }
      retval = scheme_make_pair(scheme_make_pair(scheme_intern_symbol(sqlFunctions[i].scheme_name),
						 value),
				retval);
    }

    return retval;

  }
  else if (*supported == SQL_TRUE) {
    return scheme_true;
  }
  
  return scheme_false;
}

Scheme_Object *srp_SQLGetInfo(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLUSMALLINT infoName;
  char *infoNameString;
  SRP_CONST_TYPE infoType;
  char buffer[2048];
  SQLSMALLINT actualLen;
  SRP_NAMED_TYPED_CONSTANT *p;    
  SQLUSMALLINT usmallint_value;
  SQLUINTEGER uinteger_value;
  SQLHDBC retConnectHandle;
  SQLHENV retEnvHandle;
  SQLHDESC retDescHandle;
  SQLHSTMT retStmtHandle;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-info","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-info","symbol",1,argc,argv);
  }

  infoNameString = SCHEME_SYM_VAL(argv[1]);

  p = namedTypedConstSearch(infoNameString,sqlInfo);

  if (p == NULL) {
    scheme_signal_error("sql-get-info: invalid info type: %s",
			infoNameString);
  }

  infoName = (SQLUSMALLINT)p->val;
  infoType = p->type;

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  if (argc == 3) {
    if (strcmpi(infoNameString,"sql-driver-hdesc") == 0) {
      if (SQL_HDESCP(argv[2]) == FALSE) {
	scheme_wrong_type("sql-get-info","sql-hdesc",2,argc,argv);
      }
      
      retDescHandle = SQL_HDBC_VAL(argv[2]);

    }
    else if (strcmpi(infoNameString,"sql-driver-hstmt") == 0) {
      if (SQL_HSTMTP(argv[2]) == FALSE) {
	scheme_wrong_type("sql-get-info","sql-hstmt",2,argc,argv);
      }
      
      retDescHandle = SQL_HSTMT_VAL(argv[2]);

    }
    else {
      scheme_signal_error("sql-get-info: too many arguments "
			  "for information type %s",infoNameString);
    }
  }

  switch(infoType) {

  case sqlusmallint :

    sr = SQLGetInfo(connectionHandle,infoName,&usmallint_value,0,&actualLen);
    checkSQLReturn(sr,"sql-get-info");    

    return scheme_make_integer((long)usmallint_value);

  case sqluinteger :

    sr = SQLGetInfo(connectionHandle,infoName,&uinteger_value,0,&actualLen);
    checkSQLReturn(sr,"sql-get-info");    

    return scheme_make_integer_value_from_unsigned(uinteger_value);

  case namedusmallint :

    sr = SQLGetInfo(connectionHandle,infoName,&usmallint_value,0,&actualLen);
    checkSQLReturn(sr,"sql-get-info");    

    return scheme_intern_symbol(findSmallIntName(infoNameString,usmallint_value,
						 namedInfoSmallInts,
						 sizeray(namedInfoSmallInts)));

  case nameduinteger :

    sr = SQLGetInfo(connectionHandle,infoName,&uinteger_value,0,&actualLen);
    checkSQLReturn(sr,"sql-get-info");    

    return scheme_intern_symbol(findIntegerName(infoNameString,uinteger_value,
						namedInfoIntegers,
						sizeray(namedInfoIntegers)));

  case boolstring :

    sr = SQLGetInfo(connectionHandle,infoName,buffer,sizeray(buffer),&actualLen);
    checkSQLReturn(sr,"sql-get-info");    

    if (buffer[0] == 'Y' || buffer[0] == 'y') {
      return scheme_true;
    }
    else if (buffer[0] == 'N' || buffer[0] == 'n') {
      return scheme_false;
    }
    else {
      scheme_signal_error("sql-get-info: expected 'Y' or 'N', got %s",buffer);
    }

  case string :

    sr = SQLGetInfo(connectionHandle,infoName,buffer,sizeray(buffer),&actualLen);
    checkSQLReturn(sr,"sql-get-info");    

    return scheme_make_string(buffer);

  case bitmask :

    sr = SQLGetInfo(connectionHandle,infoName,&uinteger_value,0,&actualLen);
    checkSQLReturn(sr,"sql-get-info");    

    return bitsListFromBitMask(infoNameString,uinteger_value);

  case henv :

    {
    
      SRP_SQL_HENV *retval;

      sr = SQLGetInfo(connectionHandle,infoName,&retEnvHandle,0,&actualLen);

      checkSQLReturn(sr,"sql-get-info");    

      retval = (SRP_SQL_HENV *)scheme_malloc(sizeof(SRP_SQL_HENV));
      retval->type = sql_henv_type;
      retval->henv = retEnvHandle;

      return (Scheme_Object *)retval;

    }

  case hdbc : 

    { 

      SRP_SQL_HDBC *retval;

      sr = SQLGetInfo(connectionHandle,infoName,&retConnectHandle,0,&actualLen);

      checkSQLReturn(sr,"sql-get-info");    

      retval = (SRP_SQL_HDBC *)scheme_malloc(sizeof(SRP_SQL_HDBC));
      retval->type = sql_hdbc_type;
      retval->hdbc = retConnectHandle;

      return (Scheme_Object *)retval;

    }

  case hstmt :

    {

      SRP_SQL_HSTMT *retval;

      sr = SQLGetInfo(connectionHandle,infoName,&retStmtHandle,0,&actualLen);

      checkSQLReturn(sr,"sql-get-info");    

      retval = (SRP_SQL_HSTMT *)scheme_malloc(sizeof(SRP_SQL_HSTMT));
      retval->type = sql_hstmt_type;
      retval->hstmt = retStmtHandle;
      
      return (Scheme_Object *)retval;

    }

  case hdesc :
    
    {

      SRP_SQL_HDESC *retval;

      


      sr = SQLGetInfo(connectionHandle,infoName,&retDescHandle,0,&actualLen);

      checkSQLReturn(sr,"sql-get-info");    

      retval = (SRP_SQL_HDESC *)scheme_malloc(sizeof(SRP_SQL_HDESC));
      retval->type = sql_hdesc_type;
      retval->hdesc = retDescHandle;

      return (Scheme_Object *)retval;

    }

  }

  return scheme_void;  // keep compiler happy
}

Scheme_Object *srp_SQLGetStmtAttr(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER attribute;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  SQLINTEGER actualLen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-attr","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_INTP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-attr","int or sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-attr","sql-buffer",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_CONSTANT *p;    

    attributeString = SCHEME_SYM_VAL(argv[1]);

    p = namedConstSearch(attributeString,stmtAttributes);

    if (p == NULL) {
      scheme_signal_error("sql-get-stmt-attr: invalid statement attribute: %s",
			  attributeString);
    }

    attribute = p->val;
  }
  else {
    attribute = SCHEME_INT_VAL(argv[1]);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferLen = SQL_BUFFER_LEN(argv[2]);

  sr = SQLGetStmtAttr(stmtHandle,attribute,buffer,bufferLen,&actualLen);

  checkSQLReturn(sr,"sql-get-stmt-attr");

  return scheme_void;
}

Scheme_Object *srp_SQLGetStmtOption(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT attribute;
  SQLPOINTER buffer;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-option","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_INTP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-option","int or sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-option","sql-buffer",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_SMALL_CONSTANT *p;    

    attributeString = SCHEME_SYM_VAL(argv[1]);

    p = namedSmallConstSearch(attributeString,stmtAttributes);
  
    if (p == NULL) {
      scheme_signal_error("sql-get-stmt-attr: invalid statement attribute: %s",
			  attributeString);
    }

    attribute = p->val;
  }
  else {
    attribute = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[2]);

  sr = SQLGetStmtOption(stmtHandle,attribute,buffer);

  checkSQLReturn(sr,"sql-get-stmt-option");

  return scheme_void;
}

Scheme_Object *srp_SQLGetTypeInfo(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT dataType;
  char *dataTypeString;
  SRP_NAMED_SMALL_CONSTANT *p;    

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-type-info","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-type-info","symbol",1,argc,argv);
  }

  dataTypeString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(dataTypeString,SQLDataTypes);

  if (p == NULL) {
    scheme_signal_error("sql-get-type-info: invalid data type: %s",
			dataTypeString);
  }

  dataType = p->val;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLGetTypeInfo(stmtHandle,dataType);

  checkSQLReturn(sr,"sql-get-type-info");

  return scheme_void;
}

Scheme_Object *srp_SQLNumResultCols(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT colCount;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-num-result-cols","sql-hstmt",0,argc,argv);
  }
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLNumResultCols(stmtHandle,&colCount);

  checkSQLReturn(sr,"sql-num-result-cols");

  return scheme_make_integer(colCount);
}

Scheme_Object *srp_SQLParamData(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLPOINTER buffer;
  SRP_SQL_BUFFER *retval;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-param-data","sql-hstmt",0,argc,argv);
  }
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLParamData(stmtHandle,&buffer);

  checkSQLReturn(sr,"sql-num-result-cols");

  retval = (SRP_SQL_BUFFER *)scheme_malloc(sizeof(SRP_SQL_BUFFER));

  retval->type = sql_buffer_type;
  retval->storage = buffer;
  //  retval->length = SQL_BUFFER_UNDEFINED_LEN;

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_SQLPrepare(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *text;
  SQLINTEGER textLen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-prepare","sql-hstmt",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-prepare","string",1,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  text = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  textLen = SCHEME_STRLEN_VAL(argv[1]);

  sr = SQLPrepare(stmtHandle,text,textLen);

  checkSQLReturn(sr,"sql-prepare");

  return scheme_void;
}

Scheme_Object *srp_SQLPutData(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-put-data","sql-hstmt",0,argc,argv);
  }
  
  if (SQL_BUFFERP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-put-data","sql-buffer",1,argc,argv);
  }
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[1]);
  bufferLen = SQL_BUFFER_LEN(argv[1]);
  
  sr = SQLPutData(stmtHandle,buffer,bufferLen);

  checkSQLReturn(sr,"sql-put-data");

  return scheme_void;
}

Scheme_Object *srp_SQLRowCount(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER rowCount;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-row-count","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  sr = SQLRowCount(stmtHandle,&rowCount);

  checkSQLReturn(sr,"sql-row-count");

  return scheme_make_integer_value(rowCount);
}

Scheme_Object *srp_SQLSetConnectAttr(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLINTEGER attribute;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  
  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-connect-attr","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE && SCHEME_INTP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-connect-attr","sym or int",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-connect-attr","sql-buffer",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_CONSTANT *p;    

    attributeString = SCHEME_SYM_VAL(argv[1]);

    p = namedConstSearch(attributeString,settableConnectionAttributes);

    if (p == NULL) {
      scheme_signal_error("sql-set-connect-attr: invalid connection attribute: %s",
			  attributeString);

    }

    attribute = p->val;
  }
  else {
    attribute = SCHEME_INT_VAL(argv[1]);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[1]);
  bufferLen = SQL_BUFFER_LEN(argv[1]);

  sr = SQLSetConnectAttr(connectionHandle,attribute,buffer,bufferLen);

  checkSQLReturn(sr,"sql-set-connect-attr");

  return scheme_void;
}

Scheme_Object *srp_SQLSetConnectOption(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLUSMALLINT option;
  SQLUINTEGER value;
  
  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-connect-option","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE && isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-connect-option","sym or small-int",1,argc,argv);
  }

  if (isUnsignedInt(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-connect-option","unsigned-int",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *optionString;
    SRP_NAMED_SMALL_CONSTANT *p;    

    optionString = SCHEME_SYM_VAL(argv[1]);

    p = namedSmallConstSearch(optionString,settableConnectionAttributes);

    if (p == NULL) {
      scheme_signal_error("sql-set-connect-attr: invalid connection option: %s",
			  optionString);

    }

    option = p->val;
  }
  else {
    option = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);
  scheme_get_unsigned_int_val(argv[2],&value);

  sr = SQLSetConnectOption(connectionHandle,option,value);

  checkSQLReturn(sr,"sql-set-connect-option");

  return scheme_void;
}

Scheme_Object *srp_SQLSetCursorName(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *name;
  SQLSMALLINT nameLen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-cursor-name","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-cursor-name","string",1,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  name = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  nameLen = SCHEME_STRLEN_VAL(argv[1]);

  sr = SQLSetCursorName(stmtHandle,name,nameLen);

  checkSQLReturn(sr,"sql-set-cursor-name");

  return scheme_void;
}

Scheme_Object *srp_SQLSetDescField(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDESC descHandle;
  SQLSMALLINT recNumber;
  SQLSMALLINT fieldId;
  char *fieldIdString;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  SRP_NAMED_SMALL_CONSTANT *p;    

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","sql-hdesc",0,argc,argv);    
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","small-int",1,argc,argv);    
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","symbol",2,argc,argv);    
  }

  if (SQL_BUFFERP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","sql-buffer",3,argc,argv);    
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedSmallConstSearch(fieldIdString,fieldDescriptors);

  if (p == NULL) {
      scheme_signal_error("sql-set-desc-field: invalid field id: %s",
			  fieldIdString);

  }

  fieldId = p->val;
  
  descHandle = SQL_HDESC_VAL(argv[0]);
  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);
  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferLen = SQL_BUFFER_LEN(argv[2]);

  sr = SQLSetDescField(descHandle,recNumber,fieldId,buffer,bufferLen);

  checkSQLReturn(sr,"sql-set-desc-field");

  return scheme_void;
}

Scheme_Object *srp_SQLSetDescRec(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDESC descHandle;
  SQLSMALLINT recNumber;
  SQLSMALLINT type;
  char *typeString;
  SQLSMALLINT subType;
  char *subTypeString;
  SQLINTEGER len;
  SQLSMALLINT precision;
  SQLSMALLINT scale;
  SQLPOINTER buffer;
  SRP_NAMED_SMALL_CONSTANT *p;    
  SQLINTEGER stringLen;
  SQLINTEGER indicator;
  Scheme_Object *retval;

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","sql-hdesc",0,argc,argv);    
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","small-int",1,argc,argv);    
  }
  
  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","symbol",1,argc,argv);    
  }

  if (SCHEME_INTP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","int",3,argc,argv);    
  }

  if (isSmallInt(argv[4]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","small-int",4,argc,argv);    
  }

  if (isSmallInt(argv[5]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","small-int",5,argc,argv);    
  }

  if (isSmallInt(argv[6]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","small-int",6,argc,argv);    
  }

  if (SQL_BUFFERP(argv[7]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","sql-buffer",7,argc,argv);    
  }

  if (SCHEME_INTP(argv[8]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","int",8,argc,argv);    
  }

  if (SCHEME_INTP(argv[9]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","int",9,argc,argv);    
  }

  typeString = SCHEME_SYM_VAL(argv[2]);

  p = namedSmallConstSearch(typeString,SQLDataTypes);

  if (p == NULL) {
      scheme_signal_error("sql-set-desc-rec: invalid data type: %s",
			  typeString);

  }

  type = p->val;

  subTypeString = SCHEME_SYM_VAL(argv[3]);

  p = namedSmallConstSearch(subTypeString,datetimeIntervalCodes);
 
  if (p == NULL) {
      scheme_signal_error("sql-set-desc-rec: invalid date/time interval code: %s",
			  subTypeString);

  }

  subType = p->val;

  descHandle = SQL_HDESC_VAL(argv[0]);
  recNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);
  len = SCHEME_INT_VAL(argv[4]);
  precision = (SQLSMALLINT)SCHEME_INT_VAL(argv[5]);
  scale = (SQLSMALLINT)SCHEME_INT_VAL(argv[6]);
  buffer = SQL_BUFFER_VAL(argv[7]);
  stringLen = SCHEME_INT_VAL(argv[8]);
  indicator = SCHEME_INT_VAL(argv[9]);
  
  sr = SQLSetDescRec(descHandle,recNumber,type,subType,
		     len,precision,scale,buffer,
		     &stringLen,&indicator);
  
  checkSQLReturn(sr,"sql-set-desc-rec");

  retval = scheme_null;
  retval = scheme_make_pair(scheme_make_integer(indicator),retval);
  retval = scheme_make_pair(scheme_make_integer(stringLen),retval);

  return retval;
}

Scheme_Object *srp_SQLSetEnvAttr(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLINTEGER attribute;
  char *attributeString;
  SRP_NAMED_CONSTANT *p;
  long longAttr;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-env-attr","sql-henv",0,argc,argv);    
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) { 
    scheme_wrong_type("sql-set-env-attr","symbol",1,argc,argv);
  }

  // even though some attributes may be strings, all
  // current ones are int's

  if (SCHEME_EXACT_INTEGERP(argv[2]) == FALSE) { 
    scheme_wrong_type("sql-set-env-attr","int",2,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  p = namedConstSearch(attributeString,envAttributes);

  if (p == NULL) {
      scheme_signal_error("sql-set-env-attr: invalid environment attribute: %s",
			  attributeString);
  }

  attribute = p->val;

  envHandle = SQL_HENV_VAL(argv[0]);

  if (scheme_get_int_val(argv[2],&longAttr) == 0) {
    scheme_signal_error("int %ld is out of range",longAttr);
  }

  sr = SQLSetEnvAttr(envHandle,attribute,(SQLPOINTER)longAttr,0);

  checkSQLReturn(sr,"sql-set-env-attr");

  return scheme_void;
}

Scheme_Object *srp_SQLSetParam(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT paramNumber;
  SQLSMALLINT valueType;
  char *valTypeString;
  SQLSMALLINT paramType;
  char *paramTypeString;
  SQLUINTEGER lenPrecision;
  SQLSMALLINT paramScale;
  SQLPOINTER buffer;
  SQLINTEGER actualLen;
  SRP_NAMED_SMALL_CONSTANT *p;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-param","sql-hstmt",0,argc,argv);
  } 

  if (SCHEME_INTP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-param","int",1,argc,argv);
  } 

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-param","symbol",2,argc,argv);
  } 

  if (SCHEME_SYMBOLP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-set-param","symbol",3,argc,argv);
  } 

  if (isUnsignedInt(argv[4]) == FALSE) {
    scheme_wrong_type("sql-set-param","int",4,argc,argv);
  }

 if (isSmallInt(argv[5]) == FALSE) {
    scheme_wrong_type("sql-set-param","small-int",5,argc,argv);
  }

  if (SQL_BUFFERP(argv[6]) == FALSE) {
    scheme_wrong_type("sql-set-param","sql-buffer",6,argc,argv);
  }

  valTypeString = SCHEME_SYM_VAL(argv[2]);

  p = namedSmallConstSearch(valTypeString,CDataTypes);

  if (p == NULL) {
      scheme_signal_error("sql-set-param: invalid value type: %s",
			  valTypeString);
  }

  valueType = p->val;

  paramTypeString = SCHEME_SYM_VAL(argv[3]);

  p = namedSmallConstSearch(valTypeString,SQLDataTypes);

  if (p == NULL) {
      scheme_signal_error("sql-set-param: invalid parameter type: %s",
			  valTypeString);
  }

  paramType = p->val;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  paramNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  scheme_get_unsigned_int_val(argv[4],&lenPrecision);
  paramScale = (SQLSMALLINT)SCHEME_INT_VAL(argv[5]);
  buffer = SQL_BUFFER_VAL(argv[6]);

  sr = SQLSetParam(stmtHandle,paramNumber,valueType,paramType,
		   lenPrecision,paramScale,buffer,&actualLen);

  checkSQLReturn(sr,"sql-set-param");

  return scheme_void;
}

Scheme_Object *srp_SQLSetStmtAttr(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER attribute;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-attr","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_INTP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-attr","int or sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-attr","sql-buffer",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_CONSTANT *p;    

    attributeString = SCHEME_SYM_VAL(argv[1]);

    p = namedConstSearch(attributeString,stmtAttributes);

    if (p == NULL) {
      scheme_signal_error("sql-set-stmt-attr: invalid statement attribute: %s",
			  attributeString);
    }

    attribute = p->val;
  }
  else {
    attribute = SCHEME_INT_VAL(argv[1]);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferLen = SQL_BUFFER_LEN(argv[2]);

  sr = SQLSetStmtAttr(stmtHandle,attribute,buffer,bufferLen);

  checkSQLReturn(sr,"sql-set-stmt-attr");

  return scheme_void;
}

Scheme_Object *srp_SQLSetStmtOption(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT attribute;
  SQLUINTEGER value;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-option","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_INTP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-option","int or sym",1,argc,argv);
  }

  if (isUnsignedInt(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-attr","unsigned-int",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_SMALL_CONSTANT *p;    

    attributeString = SCHEME_SYM_VAL(argv[1]);

    p = namedSmallConstSearch(attributeString,stmtAttributes);

    if (p == NULL) {
      scheme_signal_error("sql-set-stmt-option: invalid statement attribute: %s",
			  attributeString);
    }

    attribute = p->val;
  }
  else {
    attribute = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  scheme_get_unsigned_int_val(argv[2],&value);

  sr = SQLSetStmtOption(stmtHandle,attribute,value);

  checkSQLReturn(sr,"sql-set-stmt-option");

  return scheme_void;
}

Scheme_Object *srp_SQLSpecialColumns(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT idType;
  char *idTypeString;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLSMALLINT scope;
  char *scopeString;
  SQLSMALLINT nullable;
  char *nullableString;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-special-columns","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-special-columns","symbol",1,argc,argv);
  }

  for (i = 2; i <= 4; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-special-columns","string",i,argc,argv);
    }
  }

  for (i = 5; i <= 6; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-special-columns","symbol",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  idTypeString = SCHEME_SYM_VAL(argv[1]);

  if (stricmp(idTypeString,"sql-best-rowid") == 0) {
    idType = SQL_BEST_ROWID;
  }
  else if (stricmp(idTypeString,"sql-rowver") == 0) {
    idType = SQL_ROWVER;
  }
  else {
    scheme_signal_error("sql-special-columns: invalid identifier type: %s",
			idTypeString);
  }

  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[2]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[3]);

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[4]);

  scopeString = SCHEME_SYM_VAL(argv[5]);

  if (stricmp(scopeString,"sql-scope-currow") == 0) {
    scope = SQL_SCOPE_CURROW;
  }
  else if (stricmp(scopeString,"sql-scope-transaction") == 0) {
    scope = SQL_SCOPE_TRANSACTION;
  }
  else if (stricmp(scopeString,"sql-scope-session") == 0) {
    scope = SQL_SCOPE_SESSION;
  }
  else {
    scheme_signal_error("sql-special-columns: invalid scope: %s",
			scopeString);
  }

  nullableString = SCHEME_SYM_VAL(argv[6]);

  if (stricmp(nullableString,"sql-no-nulls") == 0) {
    nullable = SQL_NO_NULLS;
  }
  else if (stricmp(nullableString,"sql-nullable") == 0) {
    nullable = SQL_NULLABLE;
  }
  else {
    scheme_signal_error("sql-special-columns: invalid nullable: %s",
			nullableString);
  }

  sr = SQLSpecialColumns(stmtHandle,idType,
			 catalogName,catalogNameLen,
			 schemaName,schemaNameLen,
			 tableName,tableNameLen,
			 scope,nullable);

  checkSQLReturn(sr,"sql-special-columns");

  return scheme_void;
}

Scheme_Object *srp_SQLStatistics(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLSMALLINT unique;
  char *uniqueString;
  SQLSMALLINT reserved;
  char *reservedString;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-statistics","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-statistics","symbol",1,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-statistics","string",i,argc,argv);
    }
  }

  for (i = 4; i <= 5; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-statistics","symbol",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

  uniqueString = SCHEME_SYM_VAL(argv[4]);

  if (stricmp(uniqueString,"sql-index-unique") == 0) {
    unique = SQL_INDEX_UNIQUE;
  }
  else if (stricmp(uniqueString,"sql-index-all") == 0) {
    unique = SQL_INDEX_ALL;
  }
  else {
    scheme_signal_error("sql-statistics: invalid uniqueness specification: %s",
			uniqueString);
  }

  reservedString = SCHEME_SYM_VAL(argv[5]);

  if (stricmp(reservedString,"sql-ensure") == 0) {
    reserved = SQL_ENSURE;
  }
  else if (stricmp(reservedString,"sql-quick") == 0) {
    reserved = SQL_QUICK;
  }
  else {
    scheme_signal_error("sql-statistics: invalid reserved specification: %s",
			reservedString);
  }

  sr = SQLStatistics(stmtHandle,
		     catalogName,catalogNameLen,
		     schemaName,schemaNameLen,
		     tableName,tableNameLen,
		     unique,reserved);

  checkSQLReturn(sr,"sql-statistics");

  return scheme_void;
}

Scheme_Object *srp_SQLTables(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLCHAR *tableType;
  SQLSMALLINT tableTypeLen;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-tables","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-tables","symbol",1,argc,argv);
  }

  for (i = 1; i <= 2; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE && 
	SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-tables","string or sym",i,argc,argv);
    }
  }

  if (SCHEME_STRINGP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-tables","string or sym",i,argc,argv);
  }

  if (SCHEME_STRINGP(argv[4]) == FALSE && 
      SCHEME_SYMBOLP(argv[4]) == FALSE) {
    scheme_wrong_type("sql-tables","string or sym",4,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  if (SCHEME_SYMBOLP(argv[1])) {
    char *catalogNameString;

    catalogNameString = SCHEME_SYM_VAL(argv[1]);
    if (stricmp(catalogNameString,"sql-all-catalogs") == 0) {
      catalogName = (SQLCHAR *)SQL_ALL_CATALOGS;
      catalogNameLen = strlen((const char *)catalogName);
    }
    else {
      scheme_signal_error("sql-tables: invalid catalog name: %s",catalogNameString);
    }
  }
  else {
    catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
    catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);
  }

  if (SCHEME_SYMBOLP(argv[2])) {
    char *schemaNameString;

    schemaNameString = SCHEME_SYM_VAL(argv[2]);
    if (stricmp(schemaNameString,"sql-all-schemas") == 0) {
      schemaName = (SQLCHAR *)SQL_ALL_SCHEMAS;
      schemaNameLen = strlen((const char *)schemaName);
    }
    else {
      scheme_signal_error("sql-tables: invalid schema name: %s",schemaNameString);
    }
  }
  else {
    schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
    schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);
  }

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

  if (SCHEME_SYMBOLP(argv[4])) {
    char *tableTypeString;

    tableTypeString = SCHEME_SYM_VAL(argv[4]);
    if (stricmp(tableTypeString,"sql-all-table-types") == 0) {
      tableType = (SQLCHAR *)SQL_ALL_TABLE_TYPES;
      tableTypeLen = strlen((const char *)tableType);
    }
    else {
      scheme_signal_error("sql-tables: invalid schema name: %s",tableTypeString);
    }
  }
  else {
    tableType = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
    tableTypeLen = SCHEME_STRLEN_VAL(argv[4]);
  }

  sr = SQLTables(stmtHandle,
		 catalogName,catalogNameLen,
		 schemaName,schemaNameLen,
		 tableName,tableNameLen,
		 tableType,tableTypeLen);

  checkSQLReturn(sr,"sql-tables");

  return scheme_void;

}

Scheme_Object *srp_SQLTransact(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLHDBC connectionHandle;
  SQLUSMALLINT action;
  char *actionString;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-transact","sql-henv",0,argc,argv);
  }

  if (SQL_HDBCP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-transact","sql-hdbc",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-transact","symbol",2,argc,argv);
  }

  actionString = SCHEME_SYM_VAL(argv[2]);

  if (stricmp(actionString,"sql-commit") == 0) {
    action = SQL_COMMIT;
  }
  else if (stricmp(actionString,"sql-rollback") == 0) {
    action = SQL_ROLLBACK;
  }
  else {
    scheme_signal_error("sql-transact: invalid completion type: %s",
			actionString);
  }

  envHandle = SQL_HENV_VAL(argv[0]);
  connectionHandle = SQL_HDBC_VAL(argv[1]);

  sr = SQLTransact(envHandle,connectionHandle,action);

  checkSQLReturn(sr,"sql-transaction");

  return scheme_void;
}

// Functions in SQLEXT.H

Scheme_Object *srp_SQLDriverConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLCHAR *inConnectString;
  SQLSMALLINT inConnectStringLen;
  SQLCHAR outConnectString[2048];
  SQLSMALLINT actualLen;
  char *completionString;
  SQLUSMALLINT completion; 

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-browse-connect","sql-hdbc",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-browse-connect","string",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-browse-connect","symbol",2,argc,argv);
  }

  completionString = SCHEME_SYM_VAL(argv[2]);

  if (stricmp(completionString,"sql-driver-prompt") == 0) {
    completion = SQL_DRIVER_PROMPT;
  }
  else if (stricmp(completionString,"sql-driver-complete") == 0) {
    completion = SQL_DRIVER_COMPLETE;
  }
  else if (stricmp(completionString,"sql-driver-complete-required") == 0) {
    completion = SQL_DRIVER_COMPLETE_REQUIRED;
  }
  else if (stricmp(completionString,"sql-driver-no-prompt") == 0) {
    completion = SQL_DRIVER_NOPROMPT;
  }
  else {
    scheme_signal_error("sql-driver-connect: invalid completion: %s",
			completionString);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);
  inConnectString = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  inConnectStringLen = SCHEME_STRLEN_VAL(argv[1]);
  
  sr = SQLDriverConnect(connectionHandle,
			GetDesktopWindow(),
			inConnectString,inConnectStringLen,
			outConnectString,sizeray(outConnectString),
			&actualLen,completion);

  checkSQLReturn(sr,"sql-driver-connect");

  return scheme_make_string((const char *)outConnectString);
}

Scheme_Object *srp_SQLBrowseConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLCHAR *inConnectString;
  SQLSMALLINT inConnectStringLen;
  SQLCHAR outConnectString[2048];
  SQLSMALLINT actualLen;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-browse-connect","sql-hdbc",0,argc,argv);
  }
  
  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-browse-connect","string",1,argc,argv);
  }
  
  connectionHandle = SQL_HDBC_VAL(argv[0]);
  inConnectString = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  inConnectStringLen = SCHEME_STRLEN_VAL(argv[1]);
  
  sr = SQLBrowseConnect(connectionHandle,
			inConnectString,inConnectStringLen,
			outConnectString,sizeray(outConnectString),
			&actualLen);

  checkSQLReturn(sr,"sql-browse-connect");

  return scheme_make_string((const char *)outConnectString);
}

Scheme_Object *srp_SQLBulkOperations(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT operation;
  char *operationString;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-bulk-operations","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-bulk-operations","symbol",1,argc,argv);
  }

  operationString = SCHEME_SYM_VAL(argv[1]);
  
  if (stricmp(operationString,"sql-add") == 0) {
    operation = SQL_ADD;
  }
  else if (stricmp(operationString,"sql-update-by-bookmark") == 0) {
    operation = SQL_UPDATE_BY_BOOKMARK;
  }
  else if (stricmp(operationString,"sql-delete-by-bookmark") == 0) {
    operation = SQL_DELETE_BY_BOOKMARK;
  }
  else if (stricmp(operationString,"sql-fetch-by-bookmark") == 0) {
    operation = SQL_FETCH_BY_BOOKMARK;
  }
  else {
    scheme_signal_error("sql-bulk-operations: invalid operation: %s",
			operationString);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLBulkOperations(stmtHandle,operation);

  checkSQLReturn(sr,"sql-bulk-operations");

  return scheme_void;
}

Scheme_Object *srp_SQLColAttributes(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT colNumber;
  SQLUSMALLINT fieldId;
  char *fieldIdString;
  char buff[2048];
  SQLSMALLINT bufflen;
  SQLINTEGER numBuffer;
  SQLSMALLINT actualLen;
  SRP_NAMED_TYPED_CONSTANT *p;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-col-attributes","sql-hstmt",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-col-attributes","small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-col-attributes","symbol",2,argc,argv);
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedTypedConstSearch(fieldIdString,colAttributesOld);

  if (p == NULL) {
    scheme_signal_error("Invalid column attribute: %s",fieldIdString);
  }
    
  fieldId = (SQLUSMALLINT)(p->val);
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);

  switch(p->type) {

  case sqlbool :

    bufflen = SQL_IS_INTEGER;
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    checkSQLReturn(sr,"sql-col-attributes");		       
    return (numBuffer == SQL_FALSE) ? scheme_false : scheme_true;

  case sqlinteger :

    bufflen = SQL_IS_INTEGER;
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    checkSQLReturn(sr,"sql-col-attributes");		       
    return scheme_make_integer_value((long)numBuffer);

  case namedinteger :

    bufflen = SQL_IS_INTEGER;
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    checkSQLReturn(sr,"sql-col-attributes");		       

    return scheme_intern_symbol(findIntegerName(fieldIdString,numBuffer,
						namedColAttrsIntegers,
						sizeray(namedColAttrsIntegers)));

  case string :

    bufflen = sizeof(buff);
    sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
			 buff,bufflen,&actualLen,&numBuffer);
    checkSQLReturn(sr,"sql-col-attributes");		       
    return scheme_make_string(buff);
  }

  scheme_signal_error("sql-col-attributes: invalid attribute type");

  return scheme_void; // keep compiler happy
} 

Scheme_Object *srp_SQLColumnPrivileges(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLCHAR *columnName;
  SQLSMALLINT columnNameLen;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-column-privileges","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 4; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-column-privileges","string",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);
  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);
  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);
  columnName = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  columnNameLen = SCHEME_STRLEN_VAL(argv[4]);

  sr = SQLColumnPrivileges(stmtHandle,
			   catalogName,catalogNameLen,
			   schemaName,schemaNameLen,
			   tableName,tableNameLen,
			   columnName,columnNameLen);

  checkSQLReturn(sr,"sql-column-privileges");

  return scheme_void;
}

Scheme_Object *srp_SQLDescribeParam(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT paramNumber;
  SQLSMALLINT dataType;
  SQLUINTEGER paramSize;
  SQLSMALLINT decimalDigits;
  SQLSMALLINT nullable;
  char *nullableString;
  char *dataTypeString;
  int i;
  Scheme_Object *retval;
  
  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-describe-param","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-describe-param","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  paramNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);

  sr = SQLDescribeParam(stmtHandle,paramNumber,
			&dataType,&paramSize,&decimalDigits,
			&nullable);

  checkSQLReturn(sr,"sql-describe-col");		       

  dataTypeString = "?";

  for (i = 0; i < sizeray(SQLDataTypes); i++) {
    if (dataType == SQLDataTypes[i].val) {
      dataTypeString = SQLDataTypes[i].scheme_name;
      break;
    }
  }

  nullableString = nullableToString(nullable);

  retval = scheme_null;
  retval = scheme_make_pair(scheme_intern_symbol(nullableString),retval);
  retval = scheme_make_pair(scheme_make_integer_value(decimalDigits),retval);
  retval = scheme_make_pair(scheme_make_integer_value_from_unsigned(paramSize),retval);
  retval = scheme_make_pair(scheme_intern_symbol(dataTypeString),retval);
  
  return retval;
}

Scheme_Object *srp_SQLExtendedFetch(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT fetchType;
  char *fetchTypeString;
  SQLINTEGER rowNumber;
  SQLUINTEGER numRows;
  SQLUSMALLINT *rowStatus;
  SRP_NAMED_SMALL_CONSTANT *p;      
  SQLINTEGER actualLen;
  SQLINTEGER maxRows;
  Scheme_Object *retval;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-extended-fetch","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-extended-fetch","symbol",1,argc,argv);
  }
  
  if (SCHEME_INTP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-extended-fetch","int",2,argc,argv);
  }
  
  fetchTypeString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(fetchTypeString,fetchOrientation);

  if (p == NULL) {
    scheme_signal_error("sql-extended-fetch: invalid fetch orientation: %s",
			fetchTypeString);
  }

  fetchType = p->val;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLGetStmtAttr(stmtHandle,SQL_ATTR_MAX_ROWS,&maxRows,0,&actualLen);

  checkSQLReturn(sr,"sql-get-stmt-attr");

  // need to keep rowStatus around until cursor closed

  rowStatus = (SQLUSMALLINT *)scheme_malloc(maxRows * sizeof(*rowStatus));
  scheme_dont_gc_ptr(rowStatus);

  rowNumber = SCHEME_INT_VAL(argv[1]);

  sr = SQLExtendedFetch(stmtHandle,fetchType,rowNumber,
			&numRows,rowStatus);

  checkSQLReturn(sr,"sql-extended-fetch");

  retval = scheme_null;
  
  for (i = numRows-1; i >= 0; i--) {
    scheme_make_pair(scheme_intern_symbol(rowStatusToString(rowStatus[i])),
                     retval);
  }

  return retval;
}

Scheme_Object *srp_SQLForeignKeys(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  SQLCHAR *foreignCatalogName;
  SQLSMALLINT foreignCatalogNameLen;
  SQLCHAR *foreignSchemaName;
  SQLSMALLINT foreignSchemaNameLen;
  SQLCHAR *foreignTableName;
  SQLSMALLINT foreignTableNameLen;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-foreign-keys","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 6; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-foreign-keys","string",i,argc,argv);
    }
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);
  
  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);
  
  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

  foreignCatalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  foreignCatalogNameLen = SCHEME_STRLEN_VAL(argv[4]);
  
  foreignSchemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[5]);
  foreignSchemaNameLen = SCHEME_STRLEN_VAL(argv[5]);
  
  foreignTableName = (SQLCHAR *)SCHEME_STR_VAL(argv[6]);
  foreignTableNameLen = SCHEME_STRLEN_VAL(argv[6]);

  sr = SQLForeignKeys(stmtHandle,
		      catalogName,catalogNameLen,
		      schemaName,schemaNameLen,
		      tableName,tableNameLen,
		      foreignCatalogName,foreignCatalogNameLen,
		      foreignSchemaName,foreignSchemaNameLen,
		      foreignTableName,foreignTableNameLen);

  checkSQLReturn(sr,"sql-foreign-keys");
  
  return scheme_void;
}

Scheme_Object *srp_SQLMoreResults(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-more-results","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLMoreResults(stmtHandle);

  checkSQLReturn(sr,"sql-more-results");  

  return scheme_void;
}

Scheme_Object *srp_SQLNativeSql(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLCHAR *inSql;
  SQLINTEGER inSqlLen;
  SQLCHAR *outSql;
  SQLINTEGER outSqlLen;
  SQLINTEGER actualLen;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-native-sql","sql-hdbc",0,argc,argv);
  }

  if (SCHEME_STRINGP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-native-sql","string",1,argc,argv);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);

  inSql = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  inSqlLen = SCHEME_STRLEN_VAL(argv[1]);

  outSqlLen = 2 * inSqlLen;
  outSql = (SQLCHAR *)scheme_malloc(outSqlLen * sizeof(*outSql));

  sr = SQLNativeSql(connectionHandle,
		    inSql,inSqlLen,
		    outSql,outSqlLen,
		    &actualLen);

  checkSQLReturn(sr,"sql-native-sql");    

  return scheme_make_string((const char *)outSql);
}

Scheme_Object *srp_SQLNumParams(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLSMALLINT numParams;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-native-sql","sql-hstmt",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);  

  sr = SQLNumParams(stmtHandle,&numParams);

  checkSQLReturn(sr,"sql-num-params");    

  return scheme_make_integer(numParams);
}

Scheme_Object *srp_SQLParamOptions(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUINTEGER cRow;
  SQLUINTEGER piRow;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-param-options","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-param-options","unsigned-int",1,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  scheme_get_unsigned_int_val(argv[1],&cRow);

  sr = SQLParamOptions(stmtHandle,cRow,&piRow);
 
  checkSQLReturn(sr,"sql-param-options");    

  return scheme_make_integer_value_from_unsigned(piRow);
}

Scheme_Object *srp_SQLPrimaryKeys(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-primary-keys","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-primary-keys","string",i,argc,argv);
    }
  } 
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

  sr = SQLPrimaryKeys(stmtHandle,
		      catalogName,catalogNameLen,
		      schemaName,schemaNameLen,
		      tableName,tableNameLen);

  checkSQLReturn(sr,"sql-primary-keys");    

  return scheme_void;
}

Scheme_Object *srp_SQLProcedureColumns(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *procName;
  SQLSMALLINT procNameLen;
  SQLCHAR *columnName;
  SQLSMALLINT columnNameLen;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-procedure-columns","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 4; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-procedure-columns","string",i,argc,argv);
    }
  } 
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  procName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  procNameLen = SCHEME_STRLEN_VAL(argv[3]);

  columnName = (SQLCHAR *)SCHEME_STR_VAL(argv[4]);
  columnNameLen = SCHEME_STRLEN_VAL(argv[4]);

  sr = SQLProcedureColumns(stmtHandle,
			   catalogName,catalogNameLen,
			   schemaName,schemaNameLen,
			   procName,procNameLen,
			   columnName,columnNameLen);

  checkSQLReturn(sr,"sql-procedure-columns");    

  return scheme_void;

}

Scheme_Object *srp_SQLProcedures(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *procName;
  SQLSMALLINT procNameLen;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-procedures","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-procedures","string",i,argc,argv);
    }
  } 
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  procName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  procNameLen = SCHEME_STRLEN_VAL(argv[3]);

  sr = SQLProcedures(stmtHandle,
		     catalogName,catalogNameLen,
		     schemaName,schemaNameLen,
		     procName,procNameLen);

  checkSQLReturn(sr,"sql-procedures");    

  return scheme_void;
}

Scheme_Object *srp_SQLSetPos(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT rowNumber;
  SQLUSMALLINT operation;
  char *operationString;
  SQLUSMALLINT lock;
  char *lockString;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-pos","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-pos","unsigned-small-int",1,argc,argv);
  }

  for (i = 2; i <= 3; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-set-pos","symbol",i,argc,argv);
    }
  }

  operationString = SCHEME_SYM_VAL(argv[2]);
  lockString = SCHEME_SYM_VAL(argv[3]);

  if (strcmpi(operationString,"sql-position") == 0) {
    operation = SQL_POSITION;
  }
  else if (strcmpi(operationString,"sql-refresh") == 0) {
    operation = SQL_REFRESH;
  }
  else if (strcmpi(operationString,"sql-update") == 0) {
    operation = SQL_UPDATE;
  }
  else if (strcmpi(operationString,"sql-delete") == 0) {
    operation = SQL_DELETE;
  }
  else {
    scheme_signal_error("sql-set-pos: invalid operation: %s",operationString);
  }

  if (strcmpi(lockString,"sql-lock-no-change") == 0) {
    lock = SQL_LOCK_NO_CHANGE;
  }
  else if (strcmpi(lockString,"sql-lock-exclusive") == 0) {
    lock = SQL_LOCK_EXCLUSIVE;
  }
  else if (strcmpi(lockString,"sql-lock-unlock") == 0) {
    lock = SQL_LOCK_UNLOCK;
  }
  else {
    scheme_signal_error("sql-set-pos: invalid lock type: %s",lockString);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  rowNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  
  sr = SQLSetPos(stmtHandle,rowNumber,operation,lock);

  checkSQLReturn(sr,"sql-set-pos");    

  return scheme_void;
}

Scheme_Object *srp_SQLTablePrivileges(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *catalogName;
  SQLSMALLINT catalogNameLen;
  SQLCHAR *schemaName;
  SQLSMALLINT schemaNameLen;
  SQLCHAR *tableName;
  SQLSMALLINT tableNameLen;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-table-privileges","sql-hstmt",0,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-table-privileges","string",i,argc,argv);
    }
  } 
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  
  catalogName = (SQLCHAR *)SCHEME_STR_VAL(argv[1]);
  catalogNameLen = SCHEME_STRLEN_VAL(argv[1]);

  schemaName = (SQLCHAR *)SCHEME_STR_VAL(argv[2]);
  schemaNameLen = SCHEME_STRLEN_VAL(argv[2]);

  tableName = (SQLCHAR *)SCHEME_STR_VAL(argv[3]);
  tableNameLen = SCHEME_STRLEN_VAL(argv[3]);

  sr = SQLTablePrivileges(stmtHandle,
			  catalogName,catalogNameLen,
			  schemaName,schemaNameLen,
			  tableName,tableNameLen);

  checkSQLReturn(sr,"sql-table-privileges");    

  return scheme_void;
}

Scheme_Object *srp_SQLDrivers(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHENV envHandle;
  SQLUSMALLINT direction;
  char *directionString;
  SQLCHAR description[2048];
  SQLCHAR attributes[2048];
  SQLSMALLINT actualLen1,actualLen2;
  Scheme_Object *retval;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-drivers","sql-henv",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-drivers","symbol",1,argc,argv);
  }

  directionString = SCHEME_SYM_VAL(argv[1]);

  if (stricmp(directionString,"sql-fetch-first") == 0) {
    direction = SQL_FETCH_FIRST;
  }
  else if (stricmp(directionString,"sql-fetch-next") == 0) {
    direction = SQL_FETCH_NEXT;
  }
  else {
    scheme_signal_error("sql-drivers: invalid direction: %s",
			directionString);
  }

  envHandle = SQL_HENV_VAL(argv[0]);
  
  sr = SQLDrivers(envHandle,direction,
		  description,sizeray(description),&actualLen1,
		  attributes,sizeray(attributes),&actualLen2);

  checkSQLReturn(sr,"sql-drivers");    

  retval = scheme_null;
  retval = scheme_make_pair(scheme_make_string((const char *)attributes),
			    retval);
  retval = scheme_make_pair(scheme_make_string((const char *)description),
			    retval);
					       
  return retval;
}

Scheme_Object *srp_SQLBindParameter(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLUSMALLINT paramNumber;
  SQLSMALLINT ioType;
  char *ioTypeString;
  SQLSMALLINT valueType;
  char *valueTypeString;
  SQLSMALLINT paramType;
  char *paramTypeString;
  SQLUINTEGER colSize;
  SQLSMALLINT decimalDigits;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  SQLINTEGER *paramLength;
  SRP_NAMED_SMALL_CONSTANT *p;      
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","sql-hstmt",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","unsigned-small-int",1,argc,argv);
  }

  for (i = 2; i <= 4; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-bind-parameter","symbol",i,argc,argv);
    }
  }

  if (isUnsignedInt(argv[5]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","unsigned-int",5,argc,argv);
  }

  if (isSmallInt(argv[6]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","small-int",6,argc,argv);
  }

  if (SQL_BUFFERP(argv[7]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","sql-buffer",7,argc,argv);
  }

  if (SQL_PARAMLENGTHP(argv[8]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","sql-paramlength",8,argc,argv);
  }

  ioTypeString = SCHEME_SYM_VAL(argv[2]);

  if (stricmp(ioTypeString,"sql-param-input") == 0) {
    ioType = SQL_PARAM_INPUT;
  }
  else if (stricmp(ioTypeString,"sql-param-input-output") == 0) {
    ioType = SQL_PARAM_INPUT_OUTPUT;
  }
  else if (stricmp(ioTypeString,"sql-param-output") == 0) {
    ioType = SQL_PARAM_OUTPUT;
  }
  else {
    scheme_signal_error("sql-bind-parameter: invalid I/O type: %s",
			ioTypeString);
  }

  valueTypeString = SCHEME_SYM_VAL(argv[3]);

  p = namedSmallConstSearch(valueTypeString,CDataTypes);

  if (p == NULL) {
    scheme_signal_error("sql-bind-parameter: invalid value type: %s",
			valueTypeString);
  }

  valueType = p->val;

  paramTypeString = SCHEME_SYM_VAL(argv[4]);

  p = namedSmallConstSearch(paramTypeString,SQLDataTypes);

  if (p == NULL) {
    scheme_signal_error("sql-bind-parameter: invalid parameter type: %s",
			paramTypeString);
  }

  paramType = p->val;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  paramNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  scheme_get_unsigned_int_val(argv[5],&colSize);
  decimalDigits = (SQLSMALLINT)SCHEME_INT_VAL(argv[6]);
  buffer = SQL_BUFFER_VAL(argv[7]);
  bufferLen = SQL_BUFFER_LEN(argv[7]);
  paramLength = SQL_PARAMLENGTH_VAL(argv[8]);

  sr = SQLBindParameter(stmtHandle,paramNumber,ioType,
			valueType,paramType,
			colSize,decimalDigits,
			buffer,bufferLen,paramLength);

  checkSQLReturn(sr,"sql-bind-param");    

  return scheme_void;			
}

Scheme_Object *srp_SQLSetScrollOptions(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  HSTMT stmtHandle;
  SQLUSMALLINT concur;
  char *concurString;
  SQLINTEGER keyset;
  SQLUSMALLINT rowset;
  SRP_NAMED_SMALL_CONSTANT *p;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-scroll-options","sql-hstmt",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-scroll-options","symbol",1,argc,argv);
  }

  // deal with argv[2] below

  if (isUnsignedSmallInt(argv[3]) == FALSE) {
    scheme_wrong_type("sql-set-scroll-options","unsigned-small-int",3,argc,argv);
  }

  concurString = SCHEME_SYM_VAL(argv[1]);

  p = namedSmallConstSearch(concurString,scrollConcurrency);

  if (p == NULL) {
    scheme_signal_error("sql-set-scroll-options: invalid concurrency: %s",
			concurString);
  }

  concur = p->val;

  rowset = (SQLUSMALLINT)SCHEME_INT_VAL(argv[3]);  

  if (SCHEME_SYMBOLP(argv[2])) {
    char *keysetString;
    SRP_NAMED_CONSTANT *q;

    keysetString = SCHEME_SYM_VAL(argv[2]);
    
    q = namedConstSearch(keysetString,scrollCursor);

    if (q == NULL) {
      scheme_signal_error("sql-set-scroll-options: invalid keyset: %s",
			  keysetString);
    }

    keyset = q->val;
  }
  else if (SCHEME_EXACT_INTEGERP(argv[2])) {
    if (scheme_get_int_val(argv[1],&keyset) == 0) {
      scheme_signal_error("sql-set-scroll-options: keyset value too large");
    }

    if (keyset < rowset) {
      scheme_signal_error("sql-set-scroll-options: keyset smaller than rowset");
    }
  }
  else {
    scheme_wrong_type("sql-set-scroll-options","symbol or int",2,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLSetScrollOptions(stmtHandle,concur,keyset,rowset);

  checkSQLReturn(sr,"sql-set-scroll-options");    

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env) {
  int i;

  sql_date_type = scheme_make_type("<sql-date>");
  sql_decimal_type = scheme_make_type("<sql-decimal>");
  sql_pointer_type = scheme_make_type("<sql-pointer>");
  sql_time_type = scheme_make_type("<sql-time>");
  sql_timestamp_type = scheme_make_type("<sql-timestamp>");
  sql_return_type = scheme_make_type("<sql-return>");
  sql_handle_type = scheme_make_type("<sql-handle>");
  sql_henv_type = scheme_make_type("<sql-henv>");
  sql_hdbc_type = scheme_make_type("<sql-hdbc>");
  sql_hstmt_type = scheme_make_type("<sql-hstmt>");
  sql_hdesc_type = scheme_make_type("<sql-hdesc>");
  sql_buffer_type = scheme_make_type("<sql-buffer>");
  sql_indicator_type = scheme_make_type("<sql-indicator>");
  sql_guid_type = scheme_make_type("<sql-guid>");
  sql_paramlength_type = scheme_make_type("<sql-paramlength>");

  namedBitsDictSort(namedInfoSmallInts);
  namedBitsDictSort(namedInfoIntegers);
  namedBitsDictSort(namedColAttrIntegers);
  namedBitsDictSort(namedColAttrsIntegers);
  namedBitsDictSort(bitMaskTable);
  namedTypedConstSort(sqlInfo);
  namedTypedConstSort(colAttributes);
  namedTypedConstSort(colAttributesOld);
  namedConstSort(sqlFunctions);
  namedConstSort(envAttributes);
  namedConstSort(diagFields);
  namedConstSort(fieldDescriptors);
  namedConstSort(settableConnectionAttributes);
  namedConstSort(fetchOrientation);
  namedConstSort(fetchScrolls);
  namedConstSort(scrollConcurrency);
  namedConstSort(scrollCursor);
  namedConstSort(fetchDirections);
  namedConstSort(buffLenTypes);
  namedConstSort(CDataTypes);
  namedConstSort(SQLDataTypes);

  for (i = 0; i < sizeray(srpPrims); i++) {
    scheme_add_prim_to_env(env,
			   srpPrims[i].c_fun,
			   srpPrims[i].name,
			   srpPrims[i].minargs,
			   srpPrims[i].maxargs);
  }

  puts("SisterPersist extension for MzScheme, "
       "Copyright (c) 1999 Rice PLT (Paul Steckler)");

  return scheme_void;
}

Scheme_Object *scheme_reload(Scheme_Env *env) {
  return scheme_void;
}
