// srptypes.h -- types for SisterPersist

// equivalent C++, SQL, and Scheme types

/*

unsigned char   SQLCHAR      scheme_char_type
signed char     SQLSCHAR     scheme_char_type
unsigned char   SQLDATE      sql_date_type
unsigned char   SQLDECIMAL   sql_decimal_type
double          SQLDOUBLE    scheme_double_type
double          SQLFLOAT     scheme_double_type
long            SQLINTEGER   scheme_integer_type
unsigned long   SQLUINTEGER  scheme_integer_type
unsigned char   SQLNUMERIC   scheme_integer_type
void *          SQLPOINTER   sql_pointer_type
float           SQLREAL      scheme_double_type
short           SQLSMALLINT  scheme_integer_type
unsigned short  SQLUSMALLINT scheme_integer_type
unsigned char   SQLTIME      sql_time_type
unsigned char   SQLTIMESTAMP sql_timestamp_type
unsigned char   SQLVARCHAR   scheme_char_type
short           SQLRETURN    sql_return_type
void*	(Win32)   SQLHANDLE    sql_handle_type
long (other)    SQLHANDLE    sql_handle_type
SQLHANDLE       SQLHENV      sql_henv_type
SQLHANDLE       SQLHDBC      sql_hdbc_type
SQLHANDLE       SQLHSTMT     sql_hstmt_type
SQLHANDLE       SQLHDESC     sql_hdesc_type

*/

/* declare Scheme types for those SQL types that have
   no existing Scheme analogue
*/

extern Scheme_Type sql_date_type;
extern Scheme_Type sql_decimal_type;
extern Scheme_Type sql_pointer_type;
extern Scheme_Type sql_time_type;
extern Scheme_Type sql_timestamp_type;
extern Scheme_Type sql_return_type;
extern Scheme_Type sql_handle_type;
extern Scheme_Type sql_henv_type;
extern Scheme_Type sql_hdbc_type;
extern Scheme_Type sql_hstmt_type;
extern Scheme_Type sql_hdesc_type;
extern Scheme_Type sql_buffer_type;
extern Scheme_Type sql_paramlength_type;

typedef struct _sql_henv_ {
  Scheme_Type type;
  SQLHENV henv;
} SRP_SQL_HENV;

typedef struct _sql_hdbc_ {
  Scheme_Type type;
  SQLHDBC hdbc;
} SRP_SQL_HDBC;

typedef struct _sql_hstmt_ {
  Scheme_Type type;
  SQLHDBC hstmt;
} SRP_SQL_HSTMT;

typedef struct _sql_hdesc_ {
  Scheme_Type type;
  SQLHDBC hdesc;
} SRP_SQL_HDESC;

typedef struct _sql_buffer_ {
  Scheme_Type type;
  void *storage;
  long length;
} SRP_SQL_BUFFER;

typedef struct _sql_paramlength_ {
  Scheme_Type type;
  SQLINTEGER *lens;
} SRP_SQL_PARAMLENGTH;

#define SQL_HENVP(o) (o->type == sql_henv_type) 
#define SQL_HENV_VAL(o) (((SRP_SQL_HENV *)o)->henv)

#define SQL_HDBCP(o) (o->type == sql_hdbc_type) 
#define SQL_HDBC_VAL(o) (((SRP_SQL_HDBC *)o)->hdbc)

#define SQL_HSTMTP(o) (o->type == sql_hstmt_type) 
#define SQL_HSTMT_VAL(o) (((SRP_SQL_HSTMT *)o)->hstmt)

#define SQL_HDESCP(o) (o->type == sql_hdesc_type) 
#define SQL_HDESC_VAL(o) (((SRP_SQL_HDESC *)o)->hdesc)

#define SQL_BUFFERP(o) (o->type == sql_buffer_type) 
#define SQL_BUFFER_VAL(o) (((SRP_SQL_BUFFER *)o)->storage)
#define SQL_BUFFER_LEN(o) (((SRP_SQL_BUFFER *)o)->length)

#define SQL_PARAMLENGTHP(o) (o->type == sql_paramlength_type) 
#define SQL_PARAMLENGTH_VAL(o) (((SRP_SQL_PARAMLENGTH *)o)->lens)

#define SQL_BUFFER_UNDEFINED_LEN  (LONG_MIN)

/* typedef unsigned char           UCHAR;
typedef signed char             SCHAR;
typedef SCHAR                   SQLSCHAR;
typedef long int                SDWORD;
typedef short int               SWORD;
typedef unsigned long int       UDWORD;
typedef unsigned short int      UWORD;
typedef UDWORD                  SQLUINTEGER;

typedef signed long             SLONG;
typedef signed short            SSHORT;
typedef unsigned long           ULONG;
typedef unsigned short          USHORT;
typedef double                  SDOUBLE;
typedef double            		LDOUBLE; 
typedef float                   SFLOAT;

typedef void*              		PTR;

typedef void*              		HENV;
typedef void*              		HDBC;
typedef void*              		HSTMT;

typedef signed short            RETCODE;

#if defined(WIN32) || defined(OS2)
typedef HWND                    SQLHWND;
#elif defined (UNIX)
typedef Widget                  SQLHWND;
#else


typedef SQLPOINTER              SQLHWND;
#endif

#ifndef	__SQLDATE
#define	__SQLDATE

  typedef struct tagDATE_STRUCT
{
        SQLSMALLINT    year;
        SQLUSMALLINT   month;
        SQLUSMALLINT   day;
} DATE_STRUCT;

typedef DATE_STRUCT	SQL_DATE_STRUCT;


/*
typedef struct tagTIME_STRUCT
{
        SQLUSMALLINT   hour;
        SQLUSMALLINT   minute;
        SQLUSMALLINT   second;
} TIME_STRUCT;

typedef TIME_STRUCT	SQL_TIME_STRUCT;

typedef struct tagTIMESTAMP_STRUCT
{
        SQLSMALLINT    year;
        SQLUSMALLINT   month;
        SQLUSMALLINT   day;
        SQLUSMALLINT   hour;
        SQLUSMALLINT   minute;
        SQLUSMALLINT   second;
        SQLUINTEGER    fraction;
} TIMESTAMP_STRUCT;

typedef TIMESTAMP_STRUCT	SQL_TIMESTAMP_STRUCT;
*/

/*
 * enumerations for DATETIME_INTERVAL_SUBCODE values for interval data types
 * these values are from SQL-92
 */

/*

typedef enum 
{
	SQL_IS_YEAR						= 1,
	SQL_IS_MONTH					= 2,
	SQL_IS_DAY						= 3,
	SQL_IS_HOUR						= 4,
	SQL_IS_MINUTE					= 5,
	SQL_IS_SECOND					= 6,
	SQL_IS_YEAR_TO_MONTH			= 7,
	SQL_IS_DAY_TO_HOUR				= 8,
	SQL_IS_DAY_TO_MINUTE			= 9,
	SQL_IS_DAY_TO_SECOND			= 10,
	SQL_IS_HOUR_TO_MINUTE			= 11,
	SQL_IS_HOUR_TO_SECOND			= 12,
	SQL_IS_MINUTE_TO_SECOND			= 13
} SQLINTERVAL;


typedef struct tagSQL_YEAR_MONTH
{
		SQLUINTEGER		year;
		SQLUINTEGER		month;
} SQL_YEAR_MONTH_STRUCT;

typedef struct tagSQL_DAY_SECOND
{
		SQLUINTEGER		day;
		SQLUINTEGER		hour;
		SQLUINTEGER		minute;
		SQLUINTEGER		second;
		SQLUINTEGER		fraction;
} SQL_DAY_SECOND_STRUCT;

typedef struct tagSQL_INTERVAL_STRUCT
{
	SQLINTERVAL		interval_type;
	SQLSMALLINT		interval_sign;
	union {
		SQL_YEAR_MONTH_STRUCT		year_month;
		SQL_DAY_SECOND_STRUCT		day_second;
	} intval;

} SQL_INTERVAL_STRUCT;


#endif	*/

/*
#define ODBCINT64	__int64

#ifdef ODBCINT64
typedef ODBCINT64	SQLBIGINT;
typedef unsigned ODBCINT64	SQLUBIGINT;
#endif 

*/

/* internal representation of numeric data type */

/*
#if (ODBCVER >= 0x0300)
#define SQL_MAX_NUMERIC_LEN		16
typedef struct tagSQL_NUMERIC_STRUCT
{
	SQLCHAR		precision;
	SQLSCHAR	scale;
	SQLCHAR		sign;
	SQLCHAR		val[SQL_MAX_NUMERIC_LEN];
} SQL_NUMERIC_STRUCT;
#endif  

#ifdef GUID_DEFINED
typedef GUID	SQLGUID;
#else

typedef struct  tagSQLGUID
{
    DWORD Data1;
    WORD Data2;
    WORD Data3;
    BYTE Data4[ 8 ];
} SQLGUID;
#endif  //

*/
/*
typedef unsigned long int       BOOKMARK;

#ifdef _WCHAR_T_DEFINED
typedef wchar_t SQLWCHAR;
#else
typedef unsigned short SQLWCHAR;
#endif

#ifdef UNICODE
typedef SQLWCHAR        SQLTCHAR;
#else
typedef SQLCHAR         SQLTCHAR;
#endif 



*/


