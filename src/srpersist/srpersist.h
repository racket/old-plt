// srpersist.h

#define sizeray(x) (sizeof(x)/sizeof(*x))

#define SRP_PRIM_DECL(f) Scheme_Object *f(int,Scheme_Object **)

#define namedConstSearch(s,array) \
     (SRP_NAMED_DATATYPE *) \
     bsearch(s,array,sizeray(array),sizeof(*array), \
	     (int (*)(const void *,const void *))keyDataTypeCmp)

#define namedConstSort(array) \
     qsort(array,sizeray(array),sizeof(array[0]), \
	   (int (*)(const void *,const void *))namedDataTypesCmp)

typedef struct _SRP_prim_ {
  Scheme_Object *(*c_fun)(int argc,Scheme_Object **);
  char *name;
  short minargs;
  short maxargs;
} SRP_PRIM;

typedef struct _named_datatype_ {
  char *scheme_name;
  int val;
} SRP_NAMED_DATATYPE;

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

