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

static SRP_PRIM srpPrims[] = {

  // from SQL.H

  { srp_SQLAllocConnect,"sql-alloc-connect",1,1 },
  { srp_SQLAllocEnv,"sql-alloc-env",0,0 },
  { srp_SQLAllocHandle,"sql-alloc-handle",2,3 },
  { srp_SQLAllocStmt,"sql-alloc-stmt",2,2 },
  { srp_SQLBindCol,"sql-bind-col",4,4 },
  { srp_SQLBindParam,"sql-bind-param",8,8 },
  { srp_SQLCancel,"sql-cancel",1,1 },
  { srp_SQLCloseCursor,"sql-close-cursor",1,1 },
  { srp_SQLColAttribute,"sql-col-attribute",7,7 },
  { srp_SQLColumns,"sql-columns",9,9 },
  { srp_SQLConnect,"sql-connect",7,7 },
  { srp_SQLCopyDesc,"sql-copy-desc",2,2 },
  { srp_SQLDataSources,"sql-data-sources",8,8 },
  { srp_SQLDescribeCol,"sql-describe-col",9,9 },
  { srp_SQLDisconnect,"sql-disconnect",1,1 },
  { srp_SQLEndTran,"sql-end-tran",3,3 },
  { srp_SQLError,"sql-error",8,8 },
  { srp_SQLExecDirect,"sql-exec-direct",3,3 },
  { srp_SQLExecute,"sql-execute",1,1 },
  { srp_SQLFetch,"sql-fetch",1,1 },
  { srp_SQLFetchScroll,"sql-fetch-scroll",3,3 },
  { srp_SQLFreeConnect,"sql-free-connect",1,1 },
  { srp_SQLFreeEnv,"sql-free-env",1,1 },
  { srp_SQLFreeHandle,"sql-free-handle",2,2 },
  { srp_SQLFreeStmt,"sql-free-stmt",2,2 },
  { srp_SQLGetConnectAttr,"sql-get-connect-attr",5,5 },
  { srp_SQLGetConnectOption,"sql-get-connect-option",3,3 },
  { srp_SQLGetCursorName,"sql-get-cursor-name",4,4 },
  { srp_SQLGetData,"sql-get-data",6,6 },
  { srp_SQLGetDescField,"sql-get-desc-field",6,6 },
  { srp_SQLGetDescRec,"sql-get-desc-rec",11,11 },
  { srp_SQLGetDiagField,"sql-get-diag-field",7,7 },
  { srp_SQLGetDiagRec,"sql-get-diag-rec",8,8 },
  { srp_SQLGetEnvAttr,"sql-get-env-attr",5,5 },
  { srp_SQLGetFunctions,"sql-get-functions",3,3 },
  { srp_SQLGetInfo,"sql-get-info",5,5 },
  { srp_SQLGetStmtAttr,"sql-get-stmt-attr",5,5 },
  { srp_SQLGetStmtOption,"sql-get-stmt-option",3,3 },
  { srp_SQLGetTypeInfo,"sql-get-type-info",2,2 },
  { srp_SQLNumResultCols,"sql-num-result-cols",2,2 },
  { srp_SQLParamData,"sql-param-data",2,2 },
  { srp_SQLPrepare,"sql-prepare",3,3 },
  { srp_SQLPutData,"sql-put-data",3,3 },
  { srp_SQLRowCount,"sql-row-count",2,2 },
  { srp_SQLSetConnectAttr,"sql-set-connect-attr",4,4 },
  { srp_SQLSetConnectOption,"sql-set-connect-option",3,3 },
  { srp_SQLSetCursorName,"sql-set-cursor-name",3,3 },
  { srp_SQLSetDescField,"sql-set-desc-field",5,5 },
  { srp_SQLSetDescRec,"sql-set-desc-rec",10,10 },
  { srp_SQLSetEnvAttr,"sql-set-env-attr",4,4 },
  { srp_SQLSetParam,"sql-set-param",8,8 },
  { srp_SQLSetStmtAttr,"sql-set-stmt-attr",4,4 },
  { srp_SQLSetStmtOption,"sql-set-stmt-option",3,3 },
  { srp_SQLSpecialColumns,"sql-special-columns",10,10 },
  { srp_SQLStatistics,"sql-statistics",9,9 },
  { srp_SQLTables,"sql-tables",9,9 },
  { srp_SQLTransact,"sql-transact",3,3 },

  // from SQLEXT.H

  { srp_SQLBrowseConnect,"sql-browse-connect",6,6 },
  { srp_SQLColAttributes,"sql-col-attributes",7,7 },
  { srp_SQLColumnPrivileges,"sql-column-privileges",9,9 },
  { srp_SQLDescribeParam,"sql-describe-param",6,6 },
  { srp_SQLExtendedFetch,"sql-extended-fetch",5,5 },
  { srp_SQLForeignKeys,"sql-foreign-keys",13,13 },
  { srp_SQLMoreResults,"sql-more-results",1,1 },
  { srp_SQLNativeSql,"sql-native-sql",6,6 },
  { srp_SQLNumParams,"sql-num-params",2,2 },
  { srp_SQLParamOptions,"sql-param-options",3,3 },
  { srp_SQLPrimaryKeys,"sql-primary-keys",7,7 },
  { srp_SQLProcedureColumns,"sql-procedure-columns",9,9 },
  { srp_SQLProcedures,"sql-procedures",7,7 },
  { srp_SQLSetPos,"sql-set-pos",4,4 },
  { srp_SQLTablePrivileges,"sql-table-privileges",7,7 },
  { srp_SQLDrivers,"sql-drivers",8,8 },
  { srp_SQLBindParameter,"sql-bind-parameter",10,10 },
  { srp_SQLSetScrollOptions,"sql-set-scroll-options",3,3 },

  // miscellany

  { srp_SQLLenBinaryAttr,"sql-len-binary-attr",1,1 },

};

// named constants 

SRP_NAMED_DATATYPE fetchOrientation[] = {
  { "sql-fetch-next", SQL_FETCH_NEXT },
  { "sql-fetch-prior", SQL_FETCH_PRIOR },
  { "sql-fetch-first", SQL_FETCH_FIRST },
  { "sql-fetch-last", SQL_FETCH_LAST },
  { "sql-fetch-absolute", SQL_FETCH_ABSOLUTE },
  { "sql-fetch-relative", SQL_FETCH_RELATIVE },
  { "sql-fetch-bookmark", SQL_FETCH_BOOKMARK },
};

SRP_NAMED_DATATYPE stmtAttributes[] = {
{ "sql_attr_app_param_desc", SQL_ATTR_APP_PARAM_DESC },
{ "sql_attr_app_row_desc", SQL_ATTR_APP_ROW_DESC },
{ "sql_attr_async_enable", SQL_ATTR_ASYNC_ENABLE },
{ "sql_attr_concurrency", SQL_ATTR_CONCURRENCY },
{ "sql_attr_cursor_scrollable", SQL_ATTR_CURSOR_SCROLLABLE },
{ "sql_attr_cursor_sensitivity", SQL_ATTR_CURSOR_SENSITIVITY },
{ "sql_attr_cursor_type", SQL_ATTR_CURSOR_TYPE },
{ "sql_attr_enable_auto_ipd", SQL_ATTR_ENABLE_AUTO_IPD },
{ "sql_attr_fetch_bookmark_ptr", SQL_ATTR_FETCH_BOOKMARK_PTR },
{ "sql_attr_imp_param_desc", SQL_ATTR_IMP_PARAM_DESC },
{ "sql_attr_imp_row_desc", SQL_ATTR_IMP_ROW_DESC },
{ "sql_attr_keyset_size", SQL_ATTR_KEYSET_SIZE },
{ "sql_attr_max_length", SQL_ATTR_MAX_LENGTH },
{ "sql_attr_max_rows", SQL_ATTR_MAX_ROWS },
{ "sql_attr_metadata_id", SQL_ATTR_METADATA_ID },
{ "sql_attr_noscan", SQL_ATTR_NOSCAN },
{ "sql_attr_param_bind_offset_ptr", SQL_ATTR_PARAM_BIND_OFFSET_PTR },
{ "sql_attr_param_bind_offset_ptr", SQL_ATTR_PARAM_BIND_OFFSET_PTR },
{ "sql_attr_param_bind_type", SQL_ATTR_PARAM_BIND_TYPE },
{ "sql_attr_param_operation_ptr", SQL_ATTR_PARAM_OPERATION_PTR },
{ "sql_attr_param_status_ptr", SQL_ATTR_PARAM_STATUS_PTR },
{ "sql_attr_params_processed_ptr", SQL_ATTR_PARAMS_PROCESSED_PTR },
{ "sql_attr_paramset_size", SQL_ATTR_PARAMSET_SIZE },
{ "sql_attr_query_timeout", SQL_ATTR_QUERY_TIMEOUT },
{ "sql_attr_retrieve_data", SQL_ATTR_RETRIEVE_DATA },
{ "sql_attr_row_array_size", SQL_ATTR_ROW_ARRAY_SIZE },
{ "sql_attr_row_bind_offset_ptr", SQL_ATTR_ROW_BIND_OFFSET_PTR },
{ "sql_attr_row_bind_type", SQL_ATTR_ROW_BIND_TYPE },
{ "sql_attr_row_number", SQL_ATTR_ROW_NUMBER },
{ "sql_attr_row_operation_ptr", SQL_ATTR_ROW_OPERATION_PTR },
{ "sql_attr_row_status_ptr", SQL_ATTR_ROW_STATUS_PTR },
{ "sql_attr_rows_fetched_ptr", SQL_ATTR_ROWS_FETCHED_PTR },
{ "sql_attr_simulate_cursor", SQL_ATTR_SIMULATE_CURSOR },
{ "sql_attr_use_bookmarks", SQL_ATTR_USE_BOOKMARKS },
};

SRP_NAMED_DATATYPE sqlInfo[] = {
  { "sql-active-environments", SQL_ACTIVE_ENVIRONMENTS },
  { "sql-getdata-extensions", SQL_GETDATA_EXTENSIONS },
  { "sql-async-mode", SQL_ASYNC_MODE },
  { "sql-info-schema-views", SQL_INFO_SCHEMA_VIEWS },
  { "sql-batch-row-count", SQL_BATCH_ROW_COUNT },
  { "sql-keyset-cursor-attributes1", SQL_KEYSET_CURSOR_ATTRIBUTES1 },
  { "sql-batch-support", SQL_BATCH_SUPPORT },
  { "sql-keyset-cursor-attributes2", SQL_KEYSET_CURSOR_ATTRIBUTES2 },
  { "sql-data-source-name", SQL_DATA_SOURCE_NAME },
  { "sql-max-async-concurrent-statements", SQL_MAX_ASYNC_CONCURRENT_STATEMENTS },
  { "sql-driver-hdbc", SQL_DRIVER_HDBC },
  { "sql-max-concurrent-activities", SQL_MAX_CONCURRENT_ACTIVITIES },
  { "sql-driver-hdesc", SQL_DRIVER_HDESC },
  { "sql-max-driver-connections", SQL_MAX_DRIVER_CONNECTIONS },
  { "sql-driver-henv", SQL_DRIVER_HENV },
  { "sql-odbc-interface-conformance", SQL_ODBC_INTERFACE_CONFORMANCE },
  { "sql-driver-hlib", SQL_DRIVER_HLIB },
  { "sql-odbc-sag-cli-conformance", SQL_ODBC_SAG_CLI_CONFORMANCE },
  { "sql-driver-hstmt", SQL_DRIVER_HSTMT },
  { "sql-odbc-ver", SQL_ODBC_VER },
  { "sql-driver-name", SQL_DRIVER_NAME },
  { "sql-param-array-row-counts", SQL_PARAM_ARRAY_ROW_COUNTS },
  { "sql-driver-odbc-ver", SQL_DRIVER_ODBC_VER },
  { "sql-param-array-selects", SQL_PARAM_ARRAY_SELECTS },
  { "sql-driver-ver", SQL_DRIVER_VER },
  { "sql-row-updates", SQL_ROW_UPDATES },
  { "sql-dynamic-cursor-attributes1", SQL_DYNAMIC_CURSOR_ATTRIBUTES1 },
  { "sql-search-pattern-escape", SQL_SEARCH_PATTERN_ESCAPE },
  { "sql-dynamic-cursor-attributes2", SQL_DYNAMIC_CURSOR_ATTRIBUTES2 },
  { "sql-server-name", SQL_SERVER_NAME },
  { "sql-forward-only-cursor-attributes1", SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 },
  { "sql-static-cursor-attributes1", SQL_STATIC_CURSOR_ATTRIBUTES1 },
  { "sql-forward-only-cursor-attributes2", SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 },
  { "sql-static-cursor-attributes2", SQL_STATIC_CURSOR_ATTRIBUTES2 },
  { "sql-file-usage", SQL_FILE_USAGE },
  { "sql-database-name", SQL_DATABASE_NAME },
  { "sql-dbms-name", SQL_DBMS_NAME },
  { "sql-dbms-ver", SQL_DBMS_VER },
  { "sql-accessible-procedures", SQL_ACCESSIBLE_PROCEDURES },
  { "sql-mult-result-sets", SQL_MULT_RESULT_SETS },
  { "sql-accessible-tables", SQL_ACCESSIBLE_TABLES },
  { "sql-multiple-active-txn", SQL_MULTIPLE_ACTIVE_TXN },
  { "sql-bookmark-persistence", SQL_BOOKMARK_PERSISTENCE },
  { "sql-need-long-data-len", SQL_NEED_LONG_DATA_LEN },
  { "sql-catalog-term", SQL_CATALOG_TERM },
  { "sql-null-collation", SQL_NULL_COLLATION },
  { "sql-collation-seq", SQL_COLLATION_SEQ },
  { "sql-procedure-term", SQL_PROCEDURE_TERM },
  { "sql-concat-null-behavior", SQL_CONCAT_NULL_BEHAVIOR },
  { "sql-schema-term", SQL_SCHEMA_TERM },
  { "sql-cursor-commit-behavior", SQL_CURSOR_COMMIT_BEHAVIOR },
  { "sql-scroll-options", SQL_SCROLL_OPTIONS },
  { "sql-cursor-rollback-behavior", SQL_CURSOR_ROLLBACK_BEHAVIOR },
  { "sql-table-term", SQL_TABLE_TERM },
  { "sql-cursor-sensitivity", SQL_CURSOR_SENSITIVITY },
  { "sql-txn-capable", SQL_TXN_CAPABLE },
  { "sql-data-source-read-only", SQL_DATA_SOURCE_READ_ONLY },
  { "sql-txn-isolation-option", SQL_TXN_ISOLATION_OPTION },
  { "sql-default-txn-isolation", SQL_DEFAULT_TXN_ISOLATION },
  { "sql-user-name", SQL_USER_NAME },
  { "sql-describe-parameter", SQL_DESCRIBE_PARAMETER },
  { "sql-aggregate-functions", SQL_AGGREGATE_FUNCTIONS },
  { "sql-drop-table", SQL_DROP_TABLE },
  { "sql-alter-domain", SQL_ALTER_DOMAIN },
  { "sql-drop-translation", SQL_DROP_TRANSLATION },
  { "sql-drop-view", SQL_DROP_VIEW },
  { "sql-alter-table", SQL_ALTER_TABLE },
  { "sql-expressions-in-orderby", SQL_EXPRESSIONS_IN_ORDERBY },
  { "sql-datetime-literals", SQL_DATETIME_LITERALS },
  { "sql-group-by", SQL_GROUP_BY },
  { "sql-catalog-location", SQL_CATALOG_LOCATION },
  { "sql-identifier-case", SQL_IDENTIFIER_CASE },
  { "sql-catalog-name", SQL_CATALOG_NAME },
  { "sql-identifier-quote-char", SQL_IDENTIFIER_QUOTE_CHAR },
  { "sql-catalog-name-separator", SQL_CATALOG_NAME_SEPARATOR },
  { "sql-index-keywords", SQL_INDEX_KEYWORDS },
  { "sql-catalog-usage", SQL_CATALOG_USAGE },
  { "sql-insert-statement", SQL_INSERT_STATEMENT },
  { "sql-column-alias", SQL_COLUMN_ALIAS },
  { "sql-integrity", SQL_INTEGRITY },
  { "sql-correlation-name", SQL_CORRELATION_NAME },
  { "sql-keywords", SQL_KEYWORDS },
  { "sql-create-assertion", SQL_CREATE_ASSERTION },
  { "sql-like-escape-clause", SQL_LIKE_ESCAPE_CLAUSE },
  { "sql-create-character-set", SQL_CREATE_CHARACTER_SET },
  { "sql-non-nullable-columns", SQL_NON_NULLABLE_COLUMNS },
  { "sql-create-collation", SQL_CREATE_COLLATION },
  { "sql-sql-conformance", SQL_SQL_CONFORMANCE },
  { "sql-create-domain", SQL_CREATE_DOMAIN },
  { "sql-oj-capabilities", SQL_OJ_CAPABILITIES },
  { "sql-create-schema", SQL_CREATE_SCHEMA },
  { "sql-order-by-columns-in-select", SQL_ORDER_BY_COLUMNS_IN_SELECT },
  { "sql-create-table", SQL_CREATE_TABLE },
  { "sql-outer-joins", SQL_OUTER_JOINS },
  { "sql-create-translation", SQL_CREATE_TRANSLATION },
  { "sql-procedures", SQL_PROCEDURES },
  { "sql-ddl-index", SQL_DDL_INDEX },
  { "sql-quoted-identifier-case", SQL_QUOTED_IDENTIFIER_CASE },
  { "sql-drop-assertion", SQL_DROP_ASSERTION },
  { "sql-schema-usage", SQL_SCHEMA_USAGE },
  { "sql-drop-character-set", SQL_DROP_CHARACTER_SET },
  { "sql-special-characters", SQL_SPECIAL_CHARACTERS },
  { "sql-drop-collation", SQL_DROP_COLLATION },
  { "sql-subqueries", SQL_SUBQUERIES },
  { "sql-drop-domain", SQL_DROP_DOMAIN },
  { "sql-union", SQL_UNION },
  { "sql-drop-schema", SQL_DROP_SCHEMA },
  { "sql-max-binary-literal-len", SQL_MAX_BINARY_LITERAL_LEN },
  { "sql-max-identifier-len", SQL_MAX_IDENTIFIER_LEN },
  { "sql-max-catalog-name-len", SQL_MAX_CATALOG_NAME_LEN },
  { "sql-max-index-size", SQL_MAX_INDEX_SIZE },
  { "sql-max-char-literal-len", SQL_MAX_CHAR_LITERAL_LEN },
  { "sql-max-procedure-name-len", SQL_MAX_PROCEDURE_NAME_LEN },
  { "sql-max-column-name-len", SQL_MAX_COLUMN_NAME_LEN },
  { "sql-max-row-size", SQL_MAX_ROW_SIZE },
  { "sql-max-columns-in-group-by", SQL_MAX_COLUMNS_IN_GROUP_BY },
  { "sql-max-row-size-includes-long", SQL_MAX_ROW_SIZE_INCLUDES_LONG },
  { "sql-max-columns-in-index", SQL_MAX_COLUMNS_IN_INDEX },
  { "sql-max-schema-name-len", SQL_MAX_SCHEMA_NAME_LEN },
  { "sql-max-columns-in-order-by", SQL_MAX_COLUMNS_IN_ORDER_BY },
  { "sql-max-statement-len", SQL_MAX_STATEMENT_LEN },
  { "sql-max-columns-in-select", SQL_MAX_COLUMNS_IN_SELECT },
  { "sql-max-table-name-len", SQL_MAX_TABLE_NAME_LEN },
  { "sql-max-columns-in-table", SQL_MAX_COLUMNS_IN_TABLE },
  { "sql-max-tables-in-select", SQL_MAX_TABLES_IN_SELECT },
  { "sql-max-cursor-name-len", SQL_MAX_CURSOR_NAME_LEN },
  { "sql-max-user-name-len", SQL_MAX_USER_NAME_LEN },
  { "sql-convert-functions", SQL_CONVERT_FUNCTIONS },
  { "sql-timedate-add-intervals", SQL_TIMEDATE_ADD_INTERVALS },
  { "sql-numeric-functions", SQL_NUMERIC_FUNCTIONS },
  { "sql-timedate-diff-intervals", SQL_TIMEDATE_DIFF_INTERVALS },
  { "sql-string-functions", SQL_STRING_FUNCTIONS },
  { "sql-timedate-functions", SQL_TIMEDATE_FUNCTIONS },
  { "sql-system-functions", SQL_SYSTEM_FUNCTIONS },
  { "sql-convert-bigint", SQL_CONVERT_BIGINT },
  { "sql-convert-longvarbinary", SQL_CONVERT_LONGVARBINARY },
  { "sql-convert-binary", SQL_CONVERT_BINARY },
  { "sql-convert-longvarchar", SQL_CONVERT_LONGVARCHAR },
  { "sql-convert-bit", SQL_CONVERT_BIT },
  { "sql-convert-numeric", SQL_CONVERT_NUMERIC },
  { "sql-convert-char", SQL_CONVERT_CHAR },
  { "sql-convert-real", SQL_CONVERT_REAL },
  { "sql-convert-date", SQL_CONVERT_DATE },
  { "sql-convert-smallint", SQL_CONVERT_SMALLINT },
  { "sql-convert-decimal", SQL_CONVERT_DECIMAL },
  { "sql-convert-time", SQL_CONVERT_TIME },
  { "sql-convert-double", SQL_CONVERT_DOUBLE },
  { "sql-convert-timestamp", SQL_CONVERT_TIMESTAMP },
  { "sql-convert-float", SQL_CONVERT_FLOAT },
  { "sql-convert-tinyint", SQL_CONVERT_TINYINT },
  { "sql-convert-integer", SQL_CONVERT_INTEGER },
  { "sql-convert-varbinary", SQL_CONVERT_VARBINARY },
  { "sql-convert-interval_year_month", SQL_CONVERT_INTERVAL_YEAR_MONTH },
  { "sql-convert-varchar", SQL_CONVERT_VARCHAR },
  { "sql-convert-interval-day-time", SQL_CONVERT_INTERVAL_DAY_TIME },
  { "sql-active-environments", SQL_ACTIVE_ENVIRONMENTS },
  { "sql-drop-assertion", SQL_DROP_ASSERTION },
  { "sql-aggregate-functions", SQL_AGGREGATE_FUNCTIONS },
  { "sql-drop-character-set", SQL_DROP_CHARACTER_SET },
  { "sql-alter-domain", SQL_ALTER_DOMAIN },
  { "sql-drop-collation", SQL_DROP_COLLATION },
  { "sql-drop-domain", SQL_DROP_DOMAIN },
  { "sql-datetime-literals", SQL_DATETIME_LITERALS },
  { "sql-drop-schema", SQL_DROP_SCHEMA },
  { "sql-async-mode", SQL_ASYNC_MODE },
  { "sql-drop-table", SQL_DROP_TABLE },
  { "sql-batch-row-count", SQL_BATCH_ROW_COUNT },
  { "sql-drop-translation", SQL_DROP_TRANSLATION },
  { "sql-batch-support", SQL_BATCH_SUPPORT },
  { "sql-drop-view", SQL_DROP_VIEW },
  { "sql-catalog-name", SQL_CATALOG_NAME },
  { "sql-dynamic-cursor-attributes1", SQL_DYNAMIC_CURSOR_ATTRIBUTES1 },
  { "sql-collation-seq", SQL_COLLATION_SEQ },
  { "sql-dynamic-cursor-attributes2", SQL_DYNAMIC_CURSOR_ATTRIBUTES2 },
  { "sql-convert-interval-year-month", SQL_CONVERT_INTERVAL_YEAR_MONTH },
  { "sql-forward-only-cursor-attributes1", SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES1 },
  { "sql-convert-interval-day-time", SQL_CONVERT_INTERVAL_DAY_TIME },
  { "sql-forward-only-cursor-attributes2", SQL_FORWARD_ONLY_CURSOR_ATTRIBUTES2 },
  { "sql-create-assertion", SQL_CREATE_ASSERTION },
  { "sql-info-schema-views", SQL_INFO_SCHEMA_VIEWS },
  { "sql-create-character-set", SQL_CREATE_CHARACTER_SET },
  { "sql-insert-statement", SQL_INSERT_STATEMENT },
  { "sql-create-collation", SQL_CREATE_COLLATION },
  { "sql-keyset-cursor-attributes1", SQL_KEYSET_CURSOR_ATTRIBUTES1 },
  { "sql-create-domain", SQL_CREATE_DOMAIN },
  { "sql-keyset-cursor-attributes2", SQL_KEYSET_CURSOR_ATTRIBUTES2 },
  { "sql-create-schema", SQL_CREATE_SCHEMA },
  { "sql-max-async-concurrent-statements", SQL_MAX_ASYNC_CONCURRENT_STATEMENTS },
  { "sql-create-table", SQL_CREATE_TABLE },
  { "sql-max-identifier-len", SQL_MAX_IDENTIFIER_LEN },
  { "sql-create-translation", SQL_CREATE_TRANSLATION },
  { "sql-param-array-row-counts", SQL_PARAM_ARRAY_ROW_COUNTS },
  { "sql-cursor-sensitivity", SQL_CURSOR_SENSITIVITY },
  { "sql-param-array-selects", SQL_PARAM_ARRAY_SELECTS },
  { "sql-ddl-index", SQL_DDL_INDEX },
  { "sql-static-cursor-attributes1", SQL_STATIC_CURSOR_ATTRIBUTES1 },
  { "sql-describe-parameter", SQL_DESCRIBE_PARAMETER },
  { "sql-static-cursor-attributes2", SQL_STATIC_CURSOR_ATTRIBUTES2 },
  { "sql-dm-ver", SQL_DM_VER },
  { "sql-xopen-cli-year", SQL_XOPEN_CLI_YEAR },
  { "sql-driver-hdesc", SQL_DRIVER_HDESC },
  { "sql-active-connections", SQL_ACTIVE_CONNECTIONS },
  { "sql-max-driver-connections", SQL_MAX_DRIVER_CONNECTIONS },
  { "sql-active-statements", SQL_ACTIVE_STATEMENTS },
  { "sql-max-concurrent-activities", SQL_MAX_CONCURRENT_ACTIVITIES },
  { "sql-max-owner-name-len", SQL_MAX_OWNER_NAME_LEN },
  { "sql-max-schema-name-len", SQL_MAX_SCHEMA_NAME_LEN },
  { "sql-max-qualifier-name-len", SQL_MAX_QUALIFIER_NAME_LEN },
  { "sql-max-catalog-name-len", SQL_MAX_CATALOG_NAME_LEN },
  { "sql-odbc-sql-opt-ief", SQL_ODBC_SQL_OPT_IEF },
  { "sql-integrity", SQL_INTEGRITY },
  { "sql-owner-term", SQL_OWNER_TERM },
  { "sql-schema-term", SQL_SCHEMA_TERM },
  { "sql-owner-usage", SQL_OWNER_USAGE },
  { "sql-schema-usage", SQL_SCHEMA_USAGE },
  { "sql-qualifier-location", SQL_QUALIFIER_LOCATION },
  { "sql-catalog-location", SQL_CATALOG_LOCATION },
  { "sql-qualifier-name-separator", SQL_QUALIFIER_NAME_SEPARATOR },
  { "sql-catalog-name-separator", SQL_CATALOG_NAME_SEPARATOR },
  { "sql-qualifier-term", SQL_QUALIFIER_TERM },
  { "sql-catalog-term", SQL_CATALOG_TERM },
  { "sql-qualifier-usage", SQL_QUALIFIER_USAGE },
  { "sql-catalog-usage", SQL_CATALOG_USAGE },
  { "sql-fetch-direction", SQL_FETCH_DIRECTION },
  { "sql-pos-operations", SQL_POS_OPERATIONS },
  { "sql-lock-types", SQL_LOCK_TYPES },
  { "sql-positioned-statements", SQL_POSITIONED_STATEMENTS },
  { "sql-odbc-api-conformance", SQL_ODBC_API_CONFORMANCE },
  { "sql-scroll-concurrency", SQL_SCROLL_CONCURRENCY },
  { "sql-odbc-sql-conformance", SQL_ODBC_SQL_CONFORMANCE },
  { "sql-static-sensitivity", SQL_STATIC_SENSITIVITY },
};

SRP_NAMED_DATATYPE sqlFunctions[] = {
  {"sql-api-sqlallochandle",SQL_API_SQLALLOCHANDLE},
  {"sql-api-sqlgetdescfield",SQL_API_SQLGETDESCFIELD},
  { "sql-api-sqlbindcol", SQL_API_SQLBINDCOL },
  { "sql-api-sqlgetdescrec", SQL_API_SQLGETDESCREC },
  { "sql-api-sqlcancel", SQL_API_SQLCANCEL },
  { "sql-api-sqlgetdiagfield", SQL_API_SQLGETDIAGFIELD },
  { "sql-api-sqlclosecursor", SQL_API_SQLCLOSECURSOR },
  { "sql-api-sqlgetdiagrec", SQL_API_SQLGETDIAGREC },
  { "sql-api-sqlcolattribute", SQL_API_SQLCOLATTRIBUTE },
  { "sql-api-sqlgetenvattr", SQL_API_SQLGETENVATTR },
  { "sql-api-sqlconnect", SQL_API_SQLCONNECT },
  { "sql-api-sqlgetfunctions", SQL_API_SQLGETFUNCTIONS },
  { "sql-api-sqlcopydesc", SQL_API_SQLCOPYDESC },
  { "sql-api-sqlgetinfo", SQL_API_SQLGETINFO },
  { "sql-api-sqldatasources", SQL_API_SQLDATASOURCES },
  { "sql-api-sqlgetstmtattr", SQL_API_SQLGETSTMTATTR },
  { "sql-api-sqldescribecol", SQL_API_SQLDESCRIBECOL },
  { "sql-api-sqlgettypeinfo", SQL_API_SQLGETTYPEINFO },
  { "sql-api-sqldisconnect", SQL_API_SQLDISCONNECT },
  { "sql-api-sqlnumresultcols", SQL_API_SQLNUMRESULTCOLS },
  { "sql-api-sqldrivers", SQL_API_SQLDRIVERS },
  { "sql-api-sqlparamdata", SQL_API_SQLPARAMDATA },
  { "sql-api-sqlendtran", SQL_API_SQLENDTRAN },
  { "sql-api-sqlprepare", SQL_API_SQLPREPARE },
  { "sql-api-sqlexecdirect", SQL_API_SQLEXECDIRECT },
  { "sql-api-sqlputdata", SQL_API_SQLPUTDATA },
  { "sql-api-sqlexecute", SQL_API_SQLEXECUTE },
  { "sql-api-sqlrowcount", SQL_API_SQLROWCOUNT },
  { "sql-api-sqlfetch", SQL_API_SQLFETCH },
  { "sql-api-sqlsetconnectattr", SQL_API_SQLSETCONNECTATTR },
  { "sql-api-sqlfetchscroll", SQL_API_SQLFETCHSCROLL },
  { "sql-api-sqlsetcursorname", SQL_API_SQLSETCURSORNAME },
  { "sql-api-sqlfreehandle", SQL_API_SQLFREEHANDLE },
  { "sql-api-sqlsetdescfield", SQL_API_SQLSETDESCFIELD },
  { "sql-api-sqlfreestmt", SQL_API_SQLFREESTMT },
  { "sql-api-sqlsetdescrec", SQL_API_SQLSETDESCREC },
  { "sql-api-sqlgetconnectattr", SQL_API_SQLGETCONNECTATTR },
  { "sql-api-sqlsetenvattr", SQL_API_SQLSETENVATTR },
  { "sql-api-sqlgetcursorname", SQL_API_SQLGETCURSORNAME },
  { "sql-api-sqlsetstmtattr", SQL_API_SQLSETSTMTATTR },
  { "sql-api-sqlgetdata", SQL_API_SQLGETDATA },
  { "sql-api-sqlcolumns", SQL_API_SQLCOLUMNS },
  { "sql-api-sqlstatistics", SQL_API_SQLSTATISTICS },
  { "sql-api-sqlspecialcolumns", SQL_API_SQLSPECIALCOLUMNS },
  { "sql-api-sqltables", SQL_API_SQLTABLES },
  { "sql-api-sqlbindparameter", SQL_API_SQLBINDPARAMETER },
  { "sql-api-sqlnativesql", SQL_API_SQLNATIVESQL },
  { "sql-api-sqlbrowseconnect", SQL_API_SQLBROWSECONNECT },
  { "sql-api-sqlnumparams", SQL_API_SQLNUMPARAMS },
  { "sql-api-sqlbulkoperations", SQL_API_SQLBULKOPERATIONS },
  { "sql-api-sqlprimarykeys", SQL_API_SQLPRIMARYKEYS },
  { "sql-api-sqlcolumnprivileges", SQL_API_SQLCOLUMNPRIVILEGES },
  { "sql-api-sqlprocedurecolumns", SQL_API_SQLPROCEDURECOLUMNS },
  { "sql-api-sqldescribeparam", SQL_API_SQLDESCRIBEPARAM },
  { "sql-api-sqlprocedures", SQL_API_SQLPROCEDURES },
  { "sql-api-sqldriverconnect", SQL_API_SQLDRIVERCONNECT },
  { "sql-api-sqlsetpos", SQL_API_SQLSETPOS },
  { "sql-api-sqlforeignkeys", SQL_API_SQLFOREIGNKEYS },
  { "sql-api-sqltableprivileges", SQL_API_SQLTABLEPRIVILEGES },
  { "sql-api-sqlmoreresults", SQL_API_SQLMORERESULTS },
};

SRP_NAMED_DATATYPE envAttributes[]= {
  { "sql-attr-connection-pooling", SQL_ATTR_CONNECTION_POOLING },
  { "sql-attr-cp-match", SQL_ATTR_CP_MATCH },
  { "sql-attr-odbc-version", SQL_ATTR_ODBC_VERSION },
  { "sql-attr-output-nts", SQL_ATTR_OUTPUT_NTS },
};

SRP_NAMED_DATATYPE diagFields[] = {
  { "sql-diag-dynamic-function",SQL_DIAG_DYNAMIC_FUNCTION },
  { "sql-diag-connection-name", SQL_DIAG_CONNECTION_NAME },
  { "sql-diag-class-origin", SQL_DIAG_CLASS_ORIGIN },
  { "sql-diag-message-text", SQL_DIAG_MESSAGE_TEXT },
  { "sql-diag-server-name", SQL_DIAG_SERVER_NAME },
  { "sql-diag-sqlstate", SQL_DIAG_SQLSTATE },
  { "sql-diag-subclass-origin", SQL_DIAG_SUBCLASS_ORIGIN },
  { "sql-diag-cursor-row-count", SQL_DIAG_CURSOR_ROW_COUNT },
  { "sql-diag-dynamic-function-code", SQL_DIAG_DYNAMIC_FUNCTION_CODE },
  { "sql-diag-number", SQL_DIAG_NUMBER },
  { "sql-diag-row-count", SQL_DIAG_ROW_COUNT },
  { "sql-diag-column-number", SQL_DIAG_COLUMN_NUMBER },
  { "sql-diag-native", SQL_DIAG_NATIVE },
  { "sql-diag-row-number", SQL_DIAG_ROW_NUMBER },
  { "sql-diag-returncode", SQL_DIAG_RETURNCODE },
};

SRP_NAMED_DATATYPE datetimeIntervalCodes[] = {
  // Datetime types

  { "sql-code-date",SQL_CODE_DATE },
  { "sql-code-time",SQL_CODE_TIME },
  { "sql-code-timestamp",SQL_CODE_TIMESTAMP },

  // Interval types
  { "sql-code-day",SQL_CODE_DAY },
  { "sql-code-day-to-hour",SQL_CODE_DAY_TO_HOUR },
  { "sql-code-day-to-minute",SQL_CODE_DAY_TO_MINUTE },
  { "sql-code-day-to-second",SQL_CODE_DAY_TO_SECOND },
  { "sql-code-hour",SQL_CODE_HOUR },
  { "sql-code-hour-to-minute",SQL_CODE_HOUR_TO_MINUTE },
  { "sql-code-hour-to-second",SQL_CODE_HOUR_TO_SECOND },
  { "sql-code-minute",SQL_CODE_MINUTE },
  { "sql-code-minute-to-second",SQL_CODE_MINUTE_TO_SECOND },
  { "sql-code-month",SQL_CODE_MONTH },
  { "sql-code-second",SQL_CODE_SECOND },
  { "sql-code-year",SQL_CODE_YEAR },
  { "sql-code-year-to-month",SQL_CODE_YEAR_TO_MONTH },
};

SRP_NAMED_DATATYPE columnDescriptors[] = {
  { "sql-column-count",SQL_COLUMN_COUNT },
  { "sql-column-name",SQL_COLUMN_NAME },
  { "sql-column-type",SQL_COLUMN_TYPE },
  { "sql-column-length",SQL_COLUMN_LENGTH },
  { "sql-column-precision",SQL_COLUMN_PRECISION },
  { "sql-column-scale",SQL_COLUMN_SCALE },
  { "sql-column-display-size",SQL_COLUMN_DISPLAY_SIZE },
  { "sql-column-nullable",SQL_COLUMN_NULLABLE },
  { "sql-column-unsigned",SQL_COLUMN_UNSIGNED },
  { "sql-column-money",SQL_COLUMN_MONEY },
  { "sql-column-updatable",SQL_COLUMN_UPDATABLE },
  { "sql-column-auto-increment",SQL_COLUMN_AUTO_INCREMENT },
  { "sql-column-case-sensitive",SQL_COLUMN_CASE_SENSITIVE },
  { "sql-column-searchable",SQL_COLUMN_SEARCHABLE },
  { "sql-column-type-name",SQL_COLUMN_TYPE_NAME },
  { "sql-column-table-name",SQL_COLUMN_TABLE_NAME },
  { "sql-column-owner-name",SQL_COLUMN_OWNER_NAME },
  { "sql-column-qualifier-name",SQL_COLUMN_QUALIFIER_NAME },
  { "sql-column-label",SQL_COLUMN_LABEL },
};

SRP_NAMED_DATATYPE fieldDescriptors[] = {
  
  // header fields

  { "sql-desc-alloc-type", SQL_DESC_ALLOC_TYPE },
  { "sql-desc-array-size", SQL_DESC_ARRAY_SIZE },
  { "sql-desc-array-status-ptr", SQL_DESC_ARRAY_STATUS_PTR },
  { "sql-desc-bind-offset-ptr", SQL_DESC_BIND_OFFSET_PTR },
  { "sql-desc-bind-type", SQL_DESC_BIND_TYPE },
  { "sql-desc-count", SQL_DESC_COUNT },
  { "sql-desc-rows-processed-ptr", SQL_DESC_ROWS_PROCESSED_PTR },

  // record fields

  { "sql-desc-auto-unique-value", SQL_DESC_AUTO_UNIQUE_VALUE },
  { "sql-desc-base-column-name", SQL_DESC_BASE_COLUMN_NAME },
  { "sql-desc-base-table-name ", SQL_DESC_BASE_TABLE_NAME },
  { "sql-desc-case-sensitive", SQL_DESC_CASE_SENSITIVE },
  { "sql-desc-catalog-name", SQL_DESC_CATALOG_NAME },
  { "sql-desc-concise-type",  SQL_DESC_CONCISE_TYPE },
  { "sql-desc-data-ptr", SQL_DESC_DATA_PTR },
  { "sql-desc-datetime-interval-code", SQL_DESC_DATETIME_INTERVAL_CODE },
  { "sql_desc_datetime_interval_precision", SQL_DESC_DATETIME_INTERVAL_PRECISION },
  { "sql-desc-display-size", SQL_DESC_DISPLAY_SIZE },
  { "sql-desc-fixed-prec-scale", SQL_DESC_FIXED_PREC_SCALE },
  { "sql-desc-indicator-ptr", SQL_DESC_INDICATOR_PTR },
  { "sql-desc-label", SQL_DESC_LABEL },
  { "sql-desc-length", SQL_DESC_LENGTH },
  { "sql-desc-literal-prefix", SQL_DESC_LITERAL_PREFIX },
  { "sql-desc-literal-suffix", SQL_DESC_LITERAL_SUFFIX },
  { "sql-desc-local-type-name", SQL_DESC_LOCAL_TYPE_NAME },
  { "sql-desc-name", SQL_DESC_NAME },
  { "sql-desc-nullable", SQL_DESC_NULLABLE },
  { "sql-desc-num-prec-radix", SQL_DESC_NUM_PREC_RADIX },
  { "sql-desc-octet-length", SQL_DESC_OCTET_LENGTH },
  { "sql-desc-octet-length-ptr", SQL_DESC_OCTET_LENGTH_PTR },
  { "sql-desc-parameter-type", SQL_DESC_PARAMETER_TYPE },
  { "sql-desc-precision", SQL_DESC_PRECISION },
  { "sql-desc-rowver", SQL_DESC_ROWVER },
  { "sql-desc-scale", SQL_DESC_SCALE },
  { "sql-desc-schema-name", SQL_DESC_SCHEMA_NAME },
  { "sql-desc-searchable", SQL_DESC_SEARCHABLE },
  { "sql-desc-table-name", SQL_DESC_TABLE_NAME },
  { "sql-desc-type", SQL_DESC_TYPE },
  { "sql-desc-type-name", SQL_DESC_TYPE_NAME },
  { "sql-desc-unnamed", SQL_DESC_UNNAMED },
  { "sql-desc-unsigned", SQL_DESC_UNSIGNED },
  { "sql-desc-updatable", SQL_DESC_UPDATABLE },
};

SRP_NAMED_DATATYPE settableConnectionAttributes[] = {
  { "sql-attr-access-mode", SQL_ATTR_ACCESS_MODE },
  { "sql-attr-async-enable", SQL_ATTR_ASYNC_ENABLE },
  { "sql-attr-autocommit", SQL_ATTR_AUTOCOMMIT },
  { "sql-attr-connection-timeout", SQL_ATTR_CONNECTION_TIMEOUT },
  { "sql-attr-current-catalog", SQL_ATTR_CURRENT_CATALOG },
  { "sql-attr-login-timeout", SQL_ATTR_LOGIN_TIMEOUT },
  { "sql-attr-metadata-id", SQL_ATTR_METADATA_ID },
  { "sql-attr-odbc-cursors", SQL_ATTR_ODBC_CURSORS },
  { "sql-attr-packet-size", SQL_ATTR_PACKET_SIZE },
  { "sql-attr-quiet-mode", SQL_ATTR_QUIET_MODE },
  { "sql-attr-trace", SQL_ATTR_TRACE },
  { "sql-attr-tracefile", SQL_ATTR_TRACEFILE },
  { "sql-attr-translate-lib", SQL_ATTR_TRANSLATE_LIB },
  { "sql-attr-translate-option", SQL_ATTR_TRANSLATE_OPTION },
  { "sql-attr-txn-isolation", SQL_ATTR_TXN_ISOLATION },
};

SRP_NAMED_DATATYPE fetchDirections[] = {
  { "sql-fetch-next", SQL_FETCH_NEXT },
  { "sql-fetch-first", SQL_FETCH_FIRST },
  { "sql-fetch-first-user", SQL_FETCH_FIRST_USER },
  { "sql-fetch-first-system", SQL_FETCH_FIRST_SYSTEM },
};

SRP_NAMED_DATATYPE fetchScrolls[] = {
  { "sql-fetch-next", SQL_FETCH_NEXT },
  { "sql-fetch-prior", SQL_FETCH_PRIOR },
  { "sql-fetch-first", SQL_FETCH_FIRST },
  { "sql-fetch-last", SQL_FETCH_LAST },
  { "sql-fetch-absolute", SQL_FETCH_ABSOLUTE },
  { "sql-fetch-relative", SQL_FETCH_RELATIVE },
  { "sql-fetch-bookmark", SQL_FETCH_BOOKMARK },
};

SRP_NAMED_DATATYPE buffLenTypes[] = {
  { "sql-is-pointer", SQL_IS_POINTER },
  { "sql-is-integer", SQL_IS_INTEGER },
  { "sql-is-uinteger", SQL_IS_UINTEGER },
  { "sql-is-smallint", SQL_IS_SMALLINT }, 
  { "sql-is-usmallint",  SQL_IS_USMALLINT },
};

SRP_NAMED_DATATYPE CDataTypes[] = {
  { "sql-c-char", SQL_C_CHAR },
  { "sql-c-long", SQL_C_LONG },
  { "sql-c-short", SQL_C_SHORT },
  { "sql-c-float", SQL_C_FLOAT },
  { "sql-c-double", SQL_C_DOUBLE },
  { "sql-c-numeric", SQL_C_NUMERIC },
  { "sql-c-date", SQL_C_DATE },
  { "sql-c-timestamp", SQL_C_TIMESTAMP },
  { "sql-c-type-date", SQL_C_TYPE_DATE },
  { "sql-c-type-time", SQL_C_TYPE_TIME },
  { "sql-c-type-timestamp", SQL_C_TYPE_TIMESTAMP },
  { "sql-c-interval-year", SQL_C_INTERVAL_YEAR },
  { "sql-c-interval-month", SQL_C_INTERVAL_MONTH },
  { "sql-c-interval-day", SQL_C_INTERVAL_DAY },
  { "sql-c-interval-hour", SQL_C_INTERVAL_HOUR },
  { "sql-c-interval-minute", SQL_C_INTERVAL_MINUTE },
  { "sql-c-interval-second", SQL_C_INTERVAL_SECOND },
  { "sql-c-interval-year-to-month", SQL_C_INTERVAL_YEAR_TO_MONTH },
  { "sql-c-interval-day-to-hour", SQL_C_INTERVAL_DAY_TO_HOUR },
  { "sql-c-interval-day-to-minute", SQL_C_INTERVAL_DAY_TO_MINUTE },
  { "sql-c-interval-day-to-second", SQL_C_INTERVAL_DAY_TO_SECOND },
  { "sql-c-interval-hour-to-minute", SQL_C_INTERVAL_HOUR_TO_MINUTE },
  { "sql-c-interval-hour-to-second", SQL_C_INTERVAL_HOUR_TO_SECOND },
  { "sql-c-interval-minute-to-second", SQL_C_INTERVAL_MINUTE_TO_SECOND },
  { "sql-c-binary", SQL_C_BINARY },
  { "sql-c-bit", SQL_C_BIT },
  { "sql-c-sbigint", SQL_C_SBIGINT },
  { "sql-c-ubigint", SQL_C_UBIGINT },
  { "sql-c-tinyint", SQL_C_TINYINT },
  { "sql-c-slong", SQL_C_SLONG },
  { "sql-c-sshort", SQL_C_SSHORT },
  { "sql-c-stinyint", SQL_C_STINYINT },
  { "sql-c-ulong", SQL_C_ULONG },
  { "sql-c-ushort", SQL_C_USHORT },
  { "sql-c-utinyint", SQL_C_UTINYINT },
  { "sql-c-bookmark", SQL_C_BOOKMARK },
  { "sql-c-guid", SQL_C_GUID },
  { "sql-c-varbookmark", SQL_C_VARBOOKMARK },
};

SRP_NAMED_DATATYPE SQLDataTypes[] = {
  { "sql-char", SQL_CHAR },
  { "sql-varchar", SQL_VARCHAR },
  { "sql-longvarchar", SQL_LONGVARCHAR },
  { "sql-wchar", SQL_WCHAR },
  { "sql-wvarchar", SQL_WVARCHAR },
  { "sql-wlongvarchar", SQL_WLONGVARCHAR }, 
  { "sql-decimal", SQL_DECIMAL },
  { "sql-numeric", SQL_NUMERIC },
  { "sql-smallint", SQL_SMALLINT },
  { "sql-integer", SQL_INTEGER },
  { "sql-real", SQL_REAL },
  { "sql-float", SQL_FLOAT },
  { "sql-double", SQL_DOUBLE },
  { "sql-bit", SQL_BIT },
  { "sql-tinyint", SQL_TINYINT },
  { "sql-bigint", SQL_BIGINT },
  { "sql-binary", SQL_BINARY },
  { "sql-varbinary", SQL_VARBINARY },
  { "sql-longvarbinary", SQL_LONGVARBINARY },
  { "sql-type-date", SQL_TYPE_DATE },
  { "sql-type-time", SQL_TYPE_TIME },
  { "sql-type-timestamp", SQL_TYPE_TIMESTAMP },
  { "sql-interval-year", SQL_INTERVAL_YEAR },
  { "sql-interval-year-to-month", SQL_INTERVAL_YEAR_TO_MONTH },
  { "sql-interval-hour", SQL_INTERVAL_HOUR },
  { "sql-interval-minute", SQL_INTERVAL_MINUTE },
  { "sql-interval-day-to-hour", SQL_INTERVAL_DAY_TO_HOUR },
  { "sql-interval-day-to-minute", SQL_INTERVAL_DAY_TO_MINUTE },
  { "sql-interval-day-to-second", SQL_INTERVAL_DAY_TO_SECOND },
  { "sql-interval-hour-to-minute", SQL_INTERVAL_HOUR_TO_MINUTE },
  { "sql-interval-hour-to-second", SQL_INTERVAL_HOUR_TO_SECOND },
  { "sql-interval-minute-to-second", SQL_INTERVAL_MINUTE_TO_SECOND },
  { "sql-guid", SQL_GUID },
};

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

int namedDataTypesCmp(SRP_NAMED_DATATYPE *p1,SRP_NAMED_DATATYPE *p2) {
  return stricmp(p1->scheme_name,p2->scheme_name);
}

int keyDataTypeCmp(char *s,SRP_NAMED_DATATYPE *p) {
  return stricmp(s,p->scheme_name);
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
    scheme_wrong_type("sql-alloc-connect","<sql_henv>",0,argc,argv);
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
    scheme_wrong_type("sql-alloc-handle","sym",0,argc,argv);
  }

  handleTypeString = SCHEME_SYM_VAL(argv[0]);

  if (stricmp(handleTypeString,"sql-handle-env") == 0) {
    SQLHENV envHandle;
    SRP_SQL_HENV *retval;
    
    if (argc > 2) {
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

  if (argc < 3) {
      scheme_signal_error("In sql-alloc-handle, when first argument is "
			  "'sql-handle-env, no other argument is allowed"); 
  }

  if (stricmp(handleTypeString,"sql-handle-dbc") == 0) {
    SQLHDBC dbcHandle;
    SRP_SQL_HDBC *retval;
    
    if (SQL_HENVP(argv[1]) == FALSE) {
      scheme_wrong_type("sql-alloc-handle","<srp-henvp>",1,argc,argv);
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
      scheme_wrong_type("sql-alloc-handle","<srp-hdbcp>",1,argc,argv);
    }

    sr = SQLAllocHandle(SQL_HANDLE_DBC,SQL_HDBC_VAL(argv[1]),&stmtHandle);

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
      scheme_wrong_type("sql-alloc-handle","<srp-hdbcp>",1,argc,argv);
    }

    sr = SQLAllocHandle(SQL_HANDLE_DBC,SQL_HDBC_VAL(argv[1]),&descHandle);

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
    scheme_wrong_type("sql-alloc-stmt","<sql_hdbc>",0,argc,argv);
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
  long retval;
  SQLHSTMT stmtHandle;
  int colNumber;
  char *typeName;
  int typeVal;
  SRP_NAMED_DATATYPE *p;
  void *buffer;
  long buflen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-bind-col","<sql_hstmt>",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-bind-col","small-int",1,argc,argv);
  }
   
  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-bind-col","sym",2,argc,argv);
  }
   
  if (SQL_BUFFERP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-bind-col","<sql-buffer>",2,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  colNumber = SCHEME_INT_VAL(argv[1]);

  typeName = SCHEME_SYM_VAL(argv[2]);

  p = namedConstSearch(typeName,CDataTypes);
 
  if (p == NULL) {
    scheme_signal_error("sql-bind-col: invalid C data type name %s",typeName);
  }

  typeVal = p->val;
   
  buffer = SQL_BUFFER_VAL(argv[3]);
  buflen = SQL_BUFFER_LEN(argv[3]);

  sr = SQLBindCol(stmtHandle,colNumber,typeVal,buffer,buflen,&retval);

  checkSQLReturn(sr,"sql-bind-col");

  return scheme_make_integer_value(retval);
}

Scheme_Object *srp_SQLBindParam(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SRP_NAMED_DATATYPE *p;
  char *cTypeName,*SQLTypeName;
  short cTypeVal,SQLTypeVal;
  short paramNum;
  short decimalDigits;
  unsigned long lengthPrecision;
  void *buffer;
  long retval;

  if (SQL_HSTMTP(argv[0])) {
    scheme_wrong_type("sql-bind-param","<sql-hstmt>",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-bind-param","small-int",1,argc,argv);
  }    

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-bind-param","sym",2,argc,argv);
  }
   
  if (SCHEME_SYMBOLP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-bind-param","sym",3,argc,argv);
  }
   
  if (SCHEME_INTP(argv[4]) == FALSE) {
    scheme_wrong_type("sql-bind-param","int",4,argc,argv);
  }
   
  if (isSmallInt(argv[5]) == FALSE) {
    scheme_wrong_type("sql-bind-param","int",5,argc,argv);
  }
   
  if (SQL_BUFFERP(argv[6]) == FALSE) {
    scheme_wrong_type("sql-bind-param","<sql-buffer>",6,argc,argv);
  }
   
  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  paramNum = (short)(SCHEME_INT_VAL(argv[1]));

  cTypeName = SCHEME_SYM_VAL(argv[2]);

  p = namedConstSearch(cTypeName,CDataTypes);
 
  if (p == NULL) {
    scheme_signal_error("sql-bind-col: invalid C data type name %s",cTypeName);
  }

  cTypeVal = p->val;

  SQLTypeName = SCHEME_SYM_VAL(argv[3]);

  p = namedConstSearch(SQLTypeName,SQLDataTypes);
 
  if (p == NULL) {
    scheme_signal_error("sql-bind-col: invalid SQL data type name %s",SQLTypeName);
  }

  SQLTypeVal = p->val;

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
    scheme_wrong_type("sql-cancel","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-close-cursor","<sql-hstmt>",0,argc,argv);
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
  SQLPOINTER charBuffer;
  SQLSMALLINT bufLen;
  SQLINTEGER numBuffer;
  SQLSMALLINT actualLen;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-col-attribute","<sql-hstmt>",0,argc,argv);
  }

  for (i = 1; i <= 2; i++) {
    if (isSmallInt(argv[1]) == FALSE) {
      scheme_wrong_type("sql-col-attribute","small-int",i,argc,argv);
    }
  }

  if (SQL_BUFFERP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-col-attribute","<sql-buffer>",3,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[4])) {
    SRP_NAMED_DATATYPE *p;
    char *buffLenSym;

    buffLenSym = SCHEME_SYM_VAL(argv[4]);

    p = namedConstSearch(buffLenSym,buffLenTypes);

    if (p == NULL) {
      scheme_signal_error("Invalid buffer type symbol: %s",buffLenSym);
    }
    
    bufLen = p->val;
  }
  else if (isSmallInt(argv[4])) {
    bufLen = (SQLSMALLINT)SCHEME_INT_VAL(argv[4]);
  }
  else {
    scheme_wrong_type("sql-col-attribute","small-int or sym",4,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLSMALLINT)SCHEME_INT_VAL(argv[1]);
  fieldId = (SQLSMALLINT)SCHEME_INT_VAL(argv[2]);
  charBuffer = SQL_BUFFER_VAL(argv[3]);

  sr = SQLColAttribute(stmtHandle,colNumber,fieldId,
		       charBuffer,bufLen,&actualLen,
		       &numBuffer);
		       
  checkSQLReturn(sr,"sql-col-attribute");		       

  return scheme_make_integer_value(numBuffer);

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
    scheme_wrong_type("sql-columns","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-columns","<sql-hdbc>",0,argc,argv);
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
      scheme_wrong_type("sql-copy-desc","<sql-hdesc>",i,argc,argv);
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
  SRP_NAMED_DATATYPE *p;
  Scheme_Object *retval;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-data-sources","<sql-henv>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-data-sources","sym",0,argc,argv);
  }

  directionString = SCHEME_SYM_VAL(argv[1]);

  p = namedConstSearch(directionString,fetchDirections);

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
    scheme_wrong_type("sql-describe-col","<sql-hstmt>",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-describe-col","<sql-hstmt>",0,argc,argv);
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
  
  return retval;
}

Scheme_Object *srp_SQLDisconnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC dbcHandle;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-disconnect","<sql-hdbc>",0,argc,argv);
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
    scheme_wrong_type("sql-end-tran","<sql-hdbc> or <sql-henv>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-end-tran","sym",1,argc,argv);
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
    scheme_wrong_type("sql-error","<sql-henv>",0,argc,argv);
  }

  if (SQL_HDBCP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-error","<sql-hdbc>",1,argc,argv);
  }

  if (SQL_HSTMTP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-error","<sql-hstmt>",2,argc,argv);
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
    scheme_wrong_type("sql-exec-direct","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-execute","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-fetch","<sql-hstmt>",0,argc,argv);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLCloseCursor(stmtHandle);

  checkSQLReturn(sr,"sql-fetch");
  
  return scheme_void;
}

Scheme_Object *srp_SQLFetchScroll(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER offset;
  char *orientationString;
  SRP_NAMED_DATATYPE *p;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-fetch-scroll","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-fetch-scroll","sym",1,argc,argv);
  }

  if (SCHEME_INTP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-fetch-scroll","int",2,argc,argv);
  }

  orientationString = SCHEME_SYM_VAL(argv[1]);

  p = namedConstSearch(orientationString,fetchScrolls);

  if (p == NULL) {
    scheme_signal_error("sql-fetch-scroll: invalid orientation: %s",
			orientationString);
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
    scheme_wrong_type("sql-free-connect","<sql-hdbc>",0,argc,argv);
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
    scheme_wrong_type("sql-free-env","<sql-henv>",0,argc,argv);
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
		      "<sql-henv> or <sql-hdbc> or <sql-hstmt> or <sql-hdesc>",
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
    scheme_wrong_type("sql-free-stmt","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-free-stmt","sym",1,argc,argv);
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
    scheme_wrong_type("sql-get-connect-attr","<sql-hdbc>",0,argc,argv);
  }
  
  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-connect-attr","sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-connect-attr","<sql-buffer>",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[3]) == FALSE) {
    SRP_NAMED_DATATYPE *p;
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
    SRP_NAMED_DATATYPE *p;
    
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
    scheme_wrong_type("sql-get-connect-option","<sql-hdbc>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE && isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-connect-option","sym or unsigned-small-int",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-connect-option","<sql-buffer>",2,argc,argv);
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
      SRP_NAMED_DATATYPE *p;
    
      p = namedConstSearch(optionString,settableConnectionAttributes);
  
      if (p == NULL) {
	scheme_signal_error("sql-get-connect-option: invalid option: %s",
			    optionString);
      }

      option = p->val;
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
    scheme_wrong_type("sql-get-cursor-name","<sql-hstmt>",0,argc,argv);
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
  SQLSMALLINT targetType;
  char *targetTypeString;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  SQLINTEGER actualLen;
  SRP_NAMED_DATATYPE *p;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-data","<sql-hstmt>",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-data","unsigned-small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-data","sym",2,argc,argv);
  }

  targetTypeString = SCHEME_SYM_VAL(argv[2]);

  p = namedConstSearch(targetTypeString,CDataTypes);

  if (p == NULL) {
    scheme_signal_error("sql-get-data: invalid C datatype: %s",
			targetTypeString);
  }

  targetType = p->val;

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferLen = SQL_BUFFER_LEN(argv[2]);

  sr = SQLGetData(stmtHandle,colNumber,targetType,buffer,bufferLen,&actualLen);

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
  SRP_NAMED_DATATYPE *p;

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-desc-field","<sql-hdesc>",0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-desc-field","small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-desc-field","sym",2,argc,argv);
  }

  if (SQL_BUFFERP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-get-desc-field","<sql-buffer>",2,argc,argv);
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedConstSearch(fieldIdString,fieldDescriptors);

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
    scheme_wrong_type("sql-get-desc-rec","<sql-hdesc>",0,argc,argv);
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
		      "<sql-henv> or <sql-hdbc> or <sql-hstmt> or <sql-hdesc>",
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
    scheme_wrong_type("sql-get-diag-field","<sql-buffer>",3,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2])) {
    SRP_NAMED_DATATYPE *p;

    diagIdString = SCHEME_SYM_VAL(argv[2]);

    p = namedConstSearch(diagIdString,diagFields);

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
		      "<sql-henv> or <sql-hdbc> or <sql-hstmt> or <sql-hdesc>",
		      0,argc,argv);
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-diag-rec",
		      "small-int",1,argc,argv);
  }

  if (isSmallInt(argv[2]) == FALSE && SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-diag-rec","sym or small-int",2,argc,argv);
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
  char *attributeString;
  SRP_NAMED_DATATYPE *p;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  SQLINTEGER actualLen;

  if (SQL_HENVP(argv[0]) == FALSE) { 
    scheme_wrong_type("sql-get-env-attr","<sql-henv>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) { 
    scheme_wrong_type("sql-get-env-attr","sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) { 
    scheme_wrong_type("sql-get-env-attr","<sql-buffer>",2,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  p = namedConstSearch(attributeString,envAttributes);

  if (p == NULL) {
    scheme_signal_error("sql-get-env-attr: invalid environment attribute: %s",
			attributeString);
  }

  attribute = p->val;
  envHandle = SQL_HENV_VAL(argv[0]);

  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferLen = SQL_BUFFER_LEN(argv[2]);
  
  sr = SQLGetEnvAttr(envHandle,attribute,buffer,bufferLen,&actualLen);

  checkSQLReturn(sr,"sql-get-env-attr");		       

  return scheme_void;
}

Scheme_Object *srp_SQLGetFunctions(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLUSMALLINT function;
  char *functionString;
  SQLUSMALLINT supported[SQL_API_ODBC3_ALL_FUNCTIONS_SIZE];
  SRP_NAMED_DATATYPE *p;  
  int i;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-functions","<sql-hdbc>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-functions","sym",1,argc,argv);
  }

  functionString = SCHEME_SYM_VAL(argv[1]);

  p = namedConstSearch(functionString,sqlFunctions);

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
  SQLUSMALLINT infoType;
  SQLPOINTER buffer;
  SQLSMALLINT bufferLen;
  SQLSMALLINT actualLen;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-info","<sql-hdbc>",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE &&
      SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-info","unsigned-small-int or sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-info","<sql-buffer>",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *infoTypeString;
    SRP_NAMED_DATATYPE *p;    

    infoTypeString = SCHEME_SYM_VAL(argv[1]);

    p = namedConstSearch(infoTypeString,sqlInfo);

    if (p == NULL) {
      scheme_signal_error("sql-get-info: invalid info type: %s",
			  infoTypeString);
    }

    infoType = p->val;
  }
  else {
    infoType = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  }

  connectionHandle = SQL_HDBC_VAL(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferLen = (SQLSMALLINT)SQL_BUFFER_LEN(argv[2]);

  sr = SQLGetInfo(connectionHandle,infoType,
		  buffer,bufferLen,&actualLen);

  checkSQLReturn(sr,"sql-get-info");

  return scheme_void;
}

Scheme_Object *srp_SQLGetStmtAttr(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLINTEGER attribute;
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  SQLINTEGER actualLen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-attr","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_INTP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-attr","int or sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-attr","<sql-buffer>",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_DATATYPE *p;    

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
    scheme_wrong_type("sql-get-stmt-option","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_INTP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-option","int or sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-get-stmt-option","<sql-buffer>",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_DATATYPE *p;    

    attributeString = SCHEME_SYM_VAL(argv[1]);

    p = namedConstSearch(attributeString,stmtAttributes);

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
  SRP_NAMED_DATATYPE *p;    

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-get-type-info","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-get-type-info","sym",1,argc,argv);
  }

  dataTypeString = SCHEME_SYM_VAL(argv[1]);

  p = namedConstSearch(dataTypeString,SQLDataTypes);

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
    scheme_wrong_type("sql-num-result-cols","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-param-data","<sql-hstmt>",0,argc,argv);
  }
  
  stmtHandle = SQL_HSTMT_VAL(argv[0]);

  sr = SQLParamData(stmtHandle,&buffer);

  checkSQLReturn(sr,"sql-num-result-cols");

  retval = (SRP_SQL_BUFFER *)scheme_malloc(sizeof(SRP_SQL_BUFFER));

  retval->type = sql_buffer_type;
  retval->storage = buffer;
  retval->length = SQL_BUFFER_UNDEFINED_LEN;

  return (Scheme_Object *)retval;
}

Scheme_Object *srp_SQLPrepare(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHSTMT stmtHandle;
  SQLCHAR *text;
  SQLINTEGER textLen;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-prepare","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-put-data","<sql-hstmt>",0,argc,argv);
  }
  
  if (SQL_BUFFERP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-put-data","<sql-buffer>",1,argc,argv);
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
    scheme_wrong_type("sql-row-count","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-set-connect-attr","<sql-hdbc>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE && SCHEME_INTP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-connect-attr","sym or int",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-connect-attr","<sql-buffer>",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_DATATYPE *p;    

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
    scheme_wrong_type("sql-set-connect-option","<sql-hdbc>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE && isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-connect-option","sym or small-int",1,argc,argv);
  }

  if (isUnsignedInt(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-connect-option","unsigned-int",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *optionString;
    SRP_NAMED_DATATYPE *p;    

    optionString = SCHEME_SYM_VAL(argv[1]);

    p = namedConstSearch(optionString,settableConnectionAttributes);

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
    scheme_wrong_type("sql-set-cursor-name","<sql-hstmt>",0,argc,argv);
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
  SRP_NAMED_DATATYPE *p;    

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","<sql-hdesc>",0,argc,argv);    
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","small-int",1,argc,argv);    
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","sym",2,argc,argv);    
  }

  if (SQL_BUFFERP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","<sql-buffer>",3,argc,argv);    
  }

  fieldIdString = SCHEME_SYM_VAL(argv[2]);

  p = namedConstSearch(fieldIdString,fieldDescriptors);

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
  SRP_NAMED_DATATYPE *p;    
  SQLINTEGER stringLen;
  SQLINTEGER indicator;
  Scheme_Object *retval;

  if (SQL_HDESCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","<sql-hdesc>",0,argc,argv);    
  }

  if (isSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","small-int",1,argc,argv);    
  }
  
  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","sym",1,argc,argv);    
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
    scheme_wrong_type("sql-set-desc-field","<sql-buffer>",7,argc,argv);    
  }

  if (SCHEME_INTP(argv[8]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","int",8,argc,argv);    
  }

  if (SCHEME_INTP(argv[9]) == FALSE) {
    scheme_wrong_type("sql-set-desc-field","int",9,argc,argv);    
  }

  typeString = SCHEME_SYM_VAL(argv[2]);

  p = namedConstSearch(typeString,SQLDataTypes);

  if (p == NULL) {
      scheme_signal_error("sql-set-desc-rec: invalid data type: %s",
			  typeString);

  }

  type = p->val;

  subTypeString = SCHEME_SYM_VAL(argv[3]);

  p = namedConstSearch(subTypeString,datetimeIntervalCodes);

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
  SQLPOINTER buffer;
  SQLINTEGER bufferLen;
  SRP_NAMED_DATATYPE *p;

  if (SQL_HENVP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-env-attr","int",0,argc,argv);    
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) { 
    scheme_wrong_type("sql-set-env-attr","sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) { 
    scheme_wrong_type("sql-set-env-attr","<sql-buffer>",2,argc,argv);
  }

  attributeString = SCHEME_SYM_VAL(argv[1]);

  p = namedConstSearch(attributeString,envAttributes);

  if (p == NULL) {
      scheme_signal_error("sql-set-env-attr: invalid environment attribute: %s",
			  attributeString);
  }

  attribute = p->val;

  envHandle = SQL_HENV_VAL(argv[0]);
  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferLen = SQL_BUFFER_LEN(argv[2]);

  sr = SQLSetEnvAttr(envHandle,attribute,buffer,bufferLen);
  
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
  SRP_NAMED_DATATYPE *p;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-param","<sql-hstmt>",0,argc,argv);
  } 

  if (SCHEME_INTP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-param","int",1,argc,argv);
  } 

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-param","sym",2,argc,argv);
  } 

  if (SCHEME_SYMBOLP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-set-param","sym",3,argc,argv);
  } 

  if (isUnsignedInt(argv[4]) == FALSE) {
    scheme_wrong_type("sql-set-param","int",4,argc,argv);
  }

  if (isSmallInt(argv[5]) == FALSE) {
    scheme_wrong_type("sql-set-param","small-int",5,argc,argv);
  }

  if (SQL_BUFFERP(argv[6]) == FALSE) {
    scheme_wrong_type("sql-set-param","<sql-buffer>",6,argc,argv);
  }

  valTypeString = SCHEME_SYM_VAL(argv[2]);

  p = namedConstSearch(valTypeString,CDataTypes);

  if (p == NULL) {
      scheme_signal_error("sql-set-param: invalid value type: %s",
			  valTypeString);
  }

  valueType = p->val;

  paramTypeString = SCHEME_SYM_VAL(argv[3]);

  p = namedConstSearch(valTypeString,SQLDataTypes);

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
    scheme_wrong_type("sql-set-stmt-attr","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_INTP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-attr","int or sym",1,argc,argv);
  }

  if (SQL_BUFFERP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-attr","<sql-buffer>",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_DATATYPE *p;    

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
    scheme_wrong_type("sql-set-stmt-option","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_INTP(argv[1]) == FALSE && SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-option","int or sym",1,argc,argv);
  }

  if (isUnsignedInt(argv[2]) == FALSE) {
    scheme_wrong_type("sql-set-stmt-attr","unsigned-int",2,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1])) {
    char *attributeString;
    SRP_NAMED_DATATYPE *p;    

    attributeString = SCHEME_SYM_VAL(argv[1]);

    p = namedConstSearch(attributeString,stmtAttributes);

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
    scheme_wrong_type("sql-special-columns","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-special-columns","sym",1,argc,argv);
  }

  for (i = 2; i <= 4; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-special-columns","string",i,argc,argv);
    }
  }

  for (i = 5; i <= 6; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-special-columns","sym",i,argc,argv);
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
    scheme_wrong_type("sql-statistics","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-statistics","sym",1,argc,argv);
  }

  for (i = 1; i <= 3; i++) {
    if (SCHEME_STRINGP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-statistics","string",i,argc,argv);
    }
  }

  for (i = 4; i <= 5; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-statistics","sym",i,argc,argv);
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
    scheme_wrong_type("sql-tables","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-tables","sym",1,argc,argv);
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
    scheme_wrong_type("sql-transact","<sql-henv>",0,argc,argv);
  }

  if (SQL_HDBCP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-transact","<sql-hdbc>",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-transact","sym",2,argc,argv);
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

Scheme_Object *srp_SQLBrowseConnect(int argc,Scheme_Object **argv) {
  SQLRETURN sr;
  SQLHDBC connectionHandle;
  SQLCHAR *inConnectString;
  SQLSMALLINT inConnectStringLen;
  SQLCHAR outConnectString[2048];
  SQLSMALLINT actualLen;

  if (SQL_HDBCP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-browse-connect","<sql-hdbc>",0,argc,argv);
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
    scheme_wrong_type("sql-bulk-operations","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-bulk-operations","sym",1,argc,argv);
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
  SQLUSMALLINT fieldDesc;
  SQLPOINTER buffer;
  SQLSMALLINT bufferLen;
  SQLSMALLINT actualLen;
  SQLINTEGER numericAttr;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-col-attributes","<sql-hstmt>",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-col-attributes","unsigned-small-int",1,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2]) == FALSE &&
      isUnsignedSmallInt(argv[2]) == FALSE) {
    scheme_wrong_type("sql-col-attributes","sym or unsigned-small-int",2,argc,argv);
  }

  if (SQL_BUFFERP(argv[3]) == FALSE) {
    scheme_wrong_type("sql-col-attributes","<sql-buffer>",3,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[2])) { 
    SRP_NAMED_DATATYPE *p;      
    char *fieldDescString; 

    fieldDescString = SCHEME_SYM_VAL(argv[2]);

    p = namedConstSearch(fieldDescString,columnDescriptors);

    if (p == NULL) {
      scheme_signal_error("sql-col-attributes: invalid field descriptor: %s",
			  fieldDescString);
    }

    fieldDesc = p->val;
  }
  else {
    fieldDesc = (SQLUSMALLINT)SCHEME_INT_VAL(argv[2]);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  colNumber = (SQLUSMALLINT)SCHEME_INT_VAL(argv[1]);
  buffer = SQL_BUFFER_VAL(argv[2]);
  bufferLen = (SQLSMALLINT)SQL_BUFFER_LEN(argv[2]);
  numericAttr = 0;

  sr = SQLColAttributes(stmtHandle,colNumber,fieldDesc,
			buffer,bufferLen,&actualLen,&numericAttr);

  checkSQLReturn(sr,"sql-col-attributes");

  return scheme_make_integer_value(numericAttr);
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
    scheme_wrong_type("sql-column-privileges","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-describe-param","<sql-hstmt>",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-describe-param","<sql-hstmt>",0,argc,argv);
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
  SRP_NAMED_DATATYPE *p;      
  SQLINTEGER actualLen;
  SQLINTEGER maxRows;
  Scheme_Object *retval;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-extended-fetch","<sql-hstmt>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-extended-fetch","sym",1,argc,argv);
  }
  
  if (SCHEME_INTP(argv[2]) == FALSE) {
    scheme_wrong_type("sql-extended-fetch","int",2,argc,argv);
  }
  
  fetchTypeString = SCHEME_SYM_VAL(argv[1]);

  p = namedConstSearch(fetchTypeString,fetchOrientation);

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
    scheme_wrong_type("sql-foreign-keys","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-more-results","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-native-sql","<sql-hdbc>",0,argc,argv);
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
    scheme_wrong_type("sql-native-sql","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-param-options","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-primary-keys","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-procedure-columns","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-procedures","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-set-pos","<sql-hstmt>",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-set-pos","unsigned-small-int",1,argc,argv);
  }

  for (i = 2; i <= 3; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-set-pos","sym",i,argc,argv);
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
    scheme_wrong_type("sql-table-privileges","<sql-hstmt>",0,argc,argv);
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
    scheme_wrong_type("sql-drivers","<sql-henv>",0,argc,argv);
  }

  if (SCHEME_SYMBOLP(argv[1]) == FALSE) {
    scheme_wrong_type("sql-drivers","sym",1,argc,argv);
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
  SRP_NAMED_DATATYPE *p;      
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","<sql-hstmt>",0,argc,argv);
  }

  if (isUnsignedSmallInt(argv[1]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","unsigned-small-int",1,argc,argv);
  }

  for (i = 2; i <= 4; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-bind-parameter","sym",i,argc,argv);
    }
  }

  if (isUnsignedInt(argv[5]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","unsigned-int",5,argc,argv);
  }

  if (isSmallInt(argv[6]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","small-int",6,argc,argv);
  }

  if (SQL_BUFFERP(argv[7]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","<sql-buffer>",7,argc,argv);
  }

  if (SQL_PARAMLENGTHP(argv[8]) == FALSE) {
    scheme_wrong_type("sql-bind-parameter","<sql-paramlength>",8,argc,argv);
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

  p = namedConstSearch(valueTypeString,CDataTypes);

  if (p == NULL) {
    scheme_signal_error("sql-bind-parameter: invalid value type: %s",
			valueTypeString);
  }

  valueType = p->val;

  paramTypeString = SCHEME_SYM_VAL(argv[4]);

  p = namedConstSearch(paramTypeString,SQLDataTypes);

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
  char *keysetString;
  SQLUSMALLINT rowset;
  int i;

  if (SQL_HSTMTP(argv[0]) == FALSE) {
    scheme_wrong_type("sql-set-scroll-options","<sql-hstmt>",0,argc,argv);
  }

  for (i = 1; i <= 2; i++) {
    if (SCHEME_SYMBOLP(argv[i]) == FALSE) {
      scheme_wrong_type("sql-set-scroll-options","sym",i,argc,argv);
    }
  }

  if (isUnsignedSmallInt(argv[3]) == FALSE) {
    scheme_wrong_type("sql-set-scroll-options","unsigned-small-int",3,argc,argv);
  }

  concurString = SCHEME_SYM_VAL(argv[1]);
  keysetString = SCHEME_SYM_VAL(argv[2]);

  if (stricmp(keysetString,"sql-scroll-forward-only") == 0) {
    keyset = SQL_SCROLL_FORWARD_ONLY;
  }
  else if (stricmp(keysetString,"sql-scroll-keyset-driven") == 0) {
    keyset = SQL_SCROLL_KEYSET_DRIVEN;
  }
  else if (stricmp(keysetString,"sql-scroll-dynamic") == 0) {
    keyset = SQL_SCROLL_DYNAMIC;
  }
  else if (stricmp(keysetString,"sql-scroll-static") == 0) {
    keyset = SQL_SCROLL_STATIC;
  }
  else {
    scheme_signal_error("sql-set-scroll-options: invalid keyset: %s",
			keysetString);
  }

  if (stricmp(keysetString,"sql-concur-read-only") == 0) {
    concur = SQL_CONCUR_READ_ONLY;
  }
  else if (stricmp(keysetString,"sql-concur-lock") == 0) {
    concur = SQL_CONCUR_LOCK;
  }
  else if (stricmp(keysetString,"sql-concur-rowver") == 0) {
    concur = SQL_CONCUR_ROWVER;
  }
  else if (stricmp(keysetString,"sql-concur-values") == 0) {
    concur = SQL_CONCUR_VALUES;
  }
  else {
    scheme_signal_error("sql-set-scroll-options: invalid concurrency: %s",
			concurString);
  }

  stmtHandle = SQL_HSTMT_VAL(argv[0]);
  rowset = (SQLUSMALLINT)SCHEME_INT_VAL(argv[3]);

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
  sql_paramlength_type = scheme_make_type("<sql-paramlength>");

  namedConstSort(sqlInfo);
  namedConstSort(sqlFunctions);
  namedConstSort(envAttributes);
  namedConstSort(diagFields);
  namedConstSort(columnDescriptors);
  namedConstSort(fieldDescriptors);
  namedConstSort(settableConnectionAttributes);
  namedConstSort(fetchOrientation);
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
