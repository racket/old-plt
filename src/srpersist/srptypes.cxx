// srptypes.cxx

#include "stdafx.h"

#ifdef WIN32
#include <windows.h>
#endif

#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

#include "escheme.h"

#include "srptypes.h"
#include "srpersist.h"

Scheme_Type sql_date_type;
Scheme_Type sql_decimal_type;
Scheme_Type sql_pointer_type;
Scheme_Type sql_time_type;
Scheme_Type sql_timestamp_type;
Scheme_Type sql_return_type;
Scheme_Type sql_handle_type;
Scheme_Type sql_henv_type;
Scheme_Type sql_hdbc_type;
Scheme_Type sql_hstmt_type;
Scheme_Type sql_hdesc_type;
Scheme_Type sql_boxed_uint_type;
Scheme_Type sql_buffer_type;
Scheme_Type sql_length_type;
Scheme_Type sql_indicator_type;
Scheme_Type sql_row_status_type;
Scheme_Type sql_array_status_type;
Scheme_Type sql_binding_offset_type;
Scheme_Type sql_rows_processed_type;
Scheme_Type sql_octet_length_type;
Scheme_Type sql_op_parms_type;
Scheme_Type sql_guid_type;
Scheme_Type sql_paramlength_type;


