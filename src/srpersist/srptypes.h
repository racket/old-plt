// srptypes.h -- types for SisterPersist

/* declare Scheme types for those SQL types that have
   no existing Scheme analog
*/

extern Scheme_Type sql_date_type;
extern Scheme_Type sql_decimal_type;
extern Scheme_Type sql_pointer_type;
extern Scheme_Type sql_time_type;
extern Scheme_Type sql_timestamp_type;
extern Scheme_Type sql_return_type;
extern Scheme_Type sql_henv_type;
extern Scheme_Type sql_hdbc_type;
extern Scheme_Type sql_hstmt_type;
extern Scheme_Type sql_hdesc_type;
extern Scheme_Type sql_ap_desc_type;
extern Scheme_Type sql_ar_desc_type;
extern Scheme_Type sql_ip_desc_type;
extern Scheme_Type sql_ir_desc_type;
extern Scheme_Type sql_boxed_uint_type;
extern Scheme_Type sql_buffer_type;
extern Scheme_Type sql_indicator_type;
extern Scheme_Type sql_row_status_type;
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

typedef struct _sql_apdesc_ {
  Scheme_Type type;
  SQLUINTEGER handle;
} SRP_SQL_AP_DESC;

typedef struct _sql_ardesc_ {
  Scheme_Type type;
  SQLUINTEGER handle;
} SRP_SQL_AR_DESC;

typedef struct _sql_ipdesc_ {
  Scheme_Type type;
  SQLUINTEGER handle;
} SRP_SQL_IP_DESC;

typedef struct _sql_irdesc_ {
  Scheme_Type type;
  SQLUINTEGER handle;
} SRP_SQL_IR_DESC;

typedef struct _sql_boxed_uint_ {
  Scheme_Type type;
  SQLUINTEGER *pointer;
} SRP_SQL_BOXED_UINT;

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

typedef struct _sql_row_status_ {
  Scheme_Type type;
  SQLUINTEGER numRows;
  SQLUSMALLINT *values;
} SRP_SQL_ROW_STATUS;

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

#define SQL_AP_DESCP(o) (!SCHEME_INTP(o) && o->type == sql_ap_desc_type) 
#define SQL_AP_DESC_VAL(o) (((SRP_SQL_AP_DESC *)o)->handle)

#define SQL_AR_DESCP(o) (!SCHEME_INTP(o) && o->type == sql_ar_desc_type) 
#define SQL_AR_DESC_VAL(o) (((SRP_SQL_AR_DESC *)o)->handle)

#define SQL_IP_DESCP(o) (!SCHEME_INTP(o) && o->type == sql_ip_desc_type) 
#define SQL_IP_DESC_VAL(o) (((SRP_SQL_IP_DESC *)o)->handle)

#define SQL_IR_DESCP(o) (!SCHEME_INTP(o) && o->type == sql_ir_desc_type) 
#define SQL_IR_DESC_VAL(o) (((SRP_SQL_IR_DESC *)o)->handle)

#define SQL_BOXED_UINTP(o) (!SCHEME_INTP(o) && o->type == sql_boxed_uint_type) 
#define SQL_BOXED_UINT_VAL(o) (((SRP_SQL_BOXED_UINT *)o)->pointer)

#define SQL_ROW_STATUSP(o) (!SCHEME_INTP(o) && o->type == sql_row_status_type) 
#define SQL_ROW_STATUS_VAL(o) (((SRP_SQL_ROW_STATUS *)o)->values)
#define SQL_ROW_STATUS_LEN(o) (((SRP_SQL_ROW_STATUS *)o)->numRows)

#define SQL_PARAMLENGTHP(o) (!SCHEME_INTP(o) && o->type == sql_paramlength_type) 
#define SQL_PARAMLENGTH_VAL(o) (((SRP_SQL_PARAMLENGTH *)o)->lens)

#define SQL_BUFFER_UNDEFINED_LEN  (LONG_MIN)



