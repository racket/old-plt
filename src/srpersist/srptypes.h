// srptypes.h -- types for SisterPersist

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
extern Scheme_Type sql_indicator_type;
extern Scheme_Type sql_guid_type;
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
  SQLSMALLINT CDataType;
  long numElts;
  int eltSize;
} SRP_SQL_BUFFER;

typedef struct _sql_indicator_ {
  Scheme_Type type;
  SQLINTEGER value;
} SRP_SQL_INDICATOR;

typedef struct _sql_paramlength_ {
  Scheme_Type type;
  SQLINTEGER *lens;
} SRP_SQL_PARAMLENGTH;

typedef struct _sql_guid_ {
  Scheme_Type type;
  SQLGUID guid;
} SRP_SQL_GUID;

#define SQL_HENVP(o) (!SCHEME_INTP(o) && o->type == sql_henv_type) 
#define SQL_HENV_VAL(o) (((SRP_SQL_HENV *)o)->henv)

#define SQL_HDBCP(o) (!SCHEME_INTP(o) && o->type == sql_hdbc_type) 
#define SQL_HDBC_VAL(o) (((SRP_SQL_HDBC *)o)->hdbc)

#define SQL_HSTMTP(o) (!SCHEME_INTP(o) && o->type == sql_hstmt_type) 
#define SQL_HSTMT_VAL(o) (((SRP_SQL_HSTMT *)o)->hstmt)

#define SQL_HDESCP(o) (!SCHEME_INTP(o) && o->type == sql_hdesc_type) 
#define SQL_HDESC_VAL(o) (((SRP_SQL_HDESC *)o)->hdesc)

#define SQL_BUFFERP(o) (!SCHEME_INTP(o) && o->type == sql_buffer_type) 
#define SQL_BUFFER_VAL(o) (((SRP_SQL_BUFFER *)o)->storage)
#define SQL_BUFFER_NUMELTS(o) (((SRP_SQL_BUFFER *)o)->numElts)
#define SQL_BUFFER_ELTSIZE(o) (((SRP_SQL_BUFFER *)o)->eltSize)
#define SQL_BUFFER_LEN(o) (SQL_BUFFER_NUMELTS(o) * SQL_BUFFER_ELTSIZE(o))
#define SQL_BUFFER_CTYPE(o) (((SRP_SQL_BUFFER *)o)->CDataType)

#define SQL_INDICATORP(o) (!SCHEME_INTP(o) && o->type == sql_indicator_type) 
#define SQL_INDICATOR_VAL(o) (((SRP_SQL_INDICATOR *)o)->value)

#define SQL_PARAMLENGTHP(o) (!SCHEME_INTP(o) && o->type == sql_paramlength_type) 
#define SQL_PARAMLENGTH_VAL(o) (((SRP_SQL_PARAMLENGTH *)o)->lens)

#define SQL_BUFFER_UNDEFINED_LEN  (LONG_MIN)



