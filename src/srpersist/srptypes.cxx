// srptypes.cxx

#include "stdafx.h"

#ifdef WIN32
#include <windows.h>
#endif

#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

#include "escheme.h"

#include "srpersist.h"
#include "srptypes.h"

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
Scheme_Type sql_buffer_type;
Scheme_Type sql_indicator_type;
Scheme_Type sql_guid_type;
Scheme_Type sql_paramlength_type;


